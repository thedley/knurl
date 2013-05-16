"knurl.py: parsing and building structured parameterisable URL segments"

# Copyright (C) 2012-2013 Tom Hedley <tom@thedley.com>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

from __future__ import unicode_literals
try:
    from urllib.parse import quote, unquote
except ImportError:
    # python 2
    import urllib
    def unquote(v, encoding):
        return urllib.unquote(v.encode(encoding)).decode(encoding)
    def quote(v, encoding):
        return urllib.quote(v.encode(encoding)).decode(encoding)
    str = unicode

import re
import io


class Error(Exception):
    "base class for exceptions from this module"


class StructSyntaxError(Error):
    def __init__(self, message, pos, token):
        self.message = message
        self.line, self.offset = pos
        self.token = token
    def __str__(self):
        return ('%s "%s", line %d, offset %d'
                % (self.message, self.token, self.line, self.offset))


# A structure contains a list of fields. Fields are labelled objects:
# struclists or values. Match declares the set of allowed string vals
# in a field. If a value doesn't match then the whole structure is
# rejected. and the next structure in the structlist must be tried. If
# there are no structures that match, the next structure in the parent
# struclist must be tried, and so on.

#           The properties of a "segment" 
# -----------------------------------------------------

# - each node has a *natural position* and an *identifying code*.
#   if a segment is in its natrual position it doesn't need to be
#   identified by its code: it can be None.  The segment can be
#   out of its natural position in which case it must be
#   identified by its code.

# - can shift forwared on a sequence if int-indexed nodes from current
# - can have a value at this node
# - is a chain of nodes that you can shift forwared from.
# - there is a "current" node cursor.
# - can go down into a subsegment at the current node

# - can be in a void where the current node is empty (unmatched state)
# - must wait until we receive a valid code via subsegment that
#   matches the current (or previous known) position otherwise
#   keep returning empty trees.

# - know what position we are at by the number of "shifts" we have
#   received.

# - know what code corresponds to what position when we recived a
#   subsegment call with a code we know (on current pos, or
#   received before)
# - or when we receive a sequence call and can match a run of nodes in
#   the sequence.

# "+" and "." are reserved names for keycodes and keynames and have
# special meaning. ("." is nice because it is also used as the
# delimeter in the codepath, which is substituted for integers so will
# never appear literally in the map or segments)


encoding = "utf8"  # python unicode strings <-> URI octets

_reserved = ",{}[]="
_special = "".join([re.escape(a) for a in _reserved])
_tok_pat = re.compile(r"(\s+)|([%s]|[^\s%s]+)" % (_special, _special))


def _tokenise(fp):
    "iterate over structure definition tokens and their start position"
    row, col = 0, 0
    for row, line in enumerate(fp):
        col = 0
        for space, token in _tok_pat.findall(line.split("#", 1)[0]):
            if space:
                col += len(space)
            else:
                yield (row + 1, col), token
                col += len(token)
    yield (row + 1, col), "" # EOF condition


def _parse(toks, end):
    "yield (name, code, struct) tuples"
    struclist = []
    for pos, tok in toks:
        if tok.startswith("$"):
            struclist.append((pos, tok, tok[1:]))
            pos, tok = next(toks)
        elif tok == "[":
            # a set of keys that match the next structure
            struct = set()
            for pos, tok in toks:
                if tok == "]":
                    break
                if tok in _reserved:
                    raise StructSyntaxError("invalid match", pos, tok)
                struct.add(tok)
            else:
                raise StructSyntaxError("no closing brace", pos, tok)
            struclist.append((pos, tok, struct))
            pos, tok = next(toks)
        elif tok not in ["", ",", "}"]:
            # parse attribute
            code = name = tok
            pos, tok = next(toks)
            if tok == "=" or tok not in _reserved:
                code = tok
                pos, tok = next(toks)
            substruc = None
            if tok == "{":
                substruc = _parse(toks, "}")
                pos, tok = next(toks)
            struclist.append((pos, tok, (code, name, substruc)))
        elif tok != end:
            raise StructSyntaxError("invalid end of struct", pos, tok)
        elif not struclist:
            # empty structure definition is a value.
            struclist = None
        if tok == end:
            return struclist
        if tok != ",":
            raise StructSyntaxError("invalid struct", pos, tok)

def _merge(pos, tok, key, p, map, submap, substruc):
    if map is None:
        raise StructSyntaxError("map is None", pos, tok)
    if key == "+":
        for n, (c, s, st) in submap.items():
            _merge(pos, tok, n, (p + c), map, s, st)
    elif key in map:
        ep, esubmap, esubstruc = map[key]
        if (ep, esubmap, esubstruc) == (p, submap, substruc):
            return
        if submap is None:
            raise StructSyntaxError("incompat paths", pos, tok)
        #if ep != p:
        #    raise StructSyntaxError("conflicting paths", pos, tok)
        for n, (c, s, st) in submap.items():
            _merge(pos, tok, n, c, esubmap, s, st)
    else:
        map[key] = p, submap, substruc


# smap 1st elem is a tuple of codes.
def _proc(tree, labels, ndup, cdup):
    "arrange elements and substitute labels"
    if tree is None:
        return None, None, None
    elems = []
    structelems = elems
    firstelem = False
    disp = {}
    smap = {}
    cmap = {} # needed for checking consistency of overlapping codepaths
    labelval = None
    for pos, tok, item in tree:
        if isinstance(item, tuple):
            code, name, subtree = item
            subndup = ndup if name == "+" else set()
            subcdup = cdup if code == "+" else set()
            substruc, subsmap, subcmap = _proc(subtree, labels, subndup,
                                               subcdup)
            if code == "=":
                lt, lm, lc = labels.setdefault(name, ([], {}, {}))
                lt[:] = substruc
                lm.update(subsmap)
                lc.update(subcmap)
            else:
                if name != "+" and name in ndup:
                    raise StructSyntaxError("duplicated name", pos, tok)
                if code != "+" and code in cdup:
                    raise StructSyntaxError("duplicated code", pos, tok)
                ndup.add(name)
                cdup.add(code)
                elems.append((code, name, substruc))
                cp = (code,) if code != "+" else ()
                _merge(pos, tok, name, cp, smap, subsmap, substruc)
                np = (name,) if name != "+" else ()
                _merge(pos, tok, code, np, cmap, subcmap, substruc)
        elif isinstance(item, set):
            ndup = set()
            cdup = set()
            if not firstelem:
                if not elems:
                    raise StructSyntaxError("misplaced matches", pos, tok)
                code, name, subtree = elems[0]
                if subtree is not None:
                    raise StructSyntaxError("substruc on conditional key",
                                            pos, tok)
                elems[0] = code, name, disp
                ndup.add(name)
                cdup.add(code)
                firstelem = True
            elems = []
            for key in item:
                disp[key] = elems
        else:
            labelval = labels.setdefault(item, ([], {}, {}))
    if labelval:
        if firstelem or structelems:
            raise StructSyntaxError("label and structure", pos, tok)
        return labelval
    return structelems, smap, cmap


def structure(strucdef):
    tree = _parse(_tokenise(io.StringIO(strucdef)), "")
    return _proc(tree, {}, set(), set())[:2]


# Reserved chars from sub-delims in RFC3986. They should never get
# percent encoded. Some broken/old UAs touch "(" and ")", as allowed
# by older RFC2396. To prevent this requires choosing from "," ";" "="
# "&" "$" "+". RFC3986 adds the following: "!" "'" "(" ")" "*".
_segres_esc = [re.escape(r) for r in ";=()"]
_seg_pat = re.compile(r"(%s|[^%s]+)" % ("|".join(_segres_esc),
                                        "".join(_segres_esc)))


# How the codes for unlabeled tree elements are determined
# ========================================================
#
# As a structure is being unpacked, we build a picture of the implicit
# ordering of codes in the tree. It is given by the 'codepos' list.
# This is the order codes are expected to be in if they are presented
# in sequence without explicit codes.  Explicitly labeled tree
# elements define the start of a new position, allowing them to occur
# at any position in a tree. subsequent unlabeled elements always
# follow a common structure.

# This last element of codepos also tells us the 'target' code for
# elements we are getting from the tree.

# codepos is a list of codes that define the current tree ordering
# upto the target point. e.g:
#  ["a"]         a's are at (a,0)
#  ["a,"b"]      b's are at (b,0), (a,1)
#  ["a","b","c"] c's are at (c,0), (b,1), (a,2)
def _treekeys(tree):
    "trees indexed by nearest code and offset from that code"
    code, offset = None, 0
    for k, subtree in tree:
        if k is not None:
            code, offset = k, 0
        yield (code, offset), subtree
        offset += 1


def _val(value):
    if value is None:
        return [[""]]
    return [[unquote(v, encoding=encoding)] for v in value.split(",")]


def _parse_segment(itoks):
    tree = []
    code = subt = None
    value = ""
    for tok in itoks:
        if tok == "(":
            if value:
                tree.append((code, _val(value)))
                code = None
            tree.append((code, _parse_segment(itoks)))
            code = None
            value = ""
            subt = True
        elif tok == ";" or tok == ")":
            if not subt:
                tree.append((code, _val(value)))
            code = subt = None
            value = ""
            if tok == ")":
                break
        elif tok == "=":
            code = unquote(value, encoding=encoding) or ""
            subt = None
            value = ""
        else:
            value = tok
            subt = None
    if not tree or value or tok == "=" or tok == ";": 
        tree.append((code, _val(value)))
    return [tuple(_treekeys(tree))]


def _split_vals(tree):
    out = []
    for vals in tree:
        if isinstance(vals, list):
            out.extend(vals)
        elif not isinstance(vals, tuple):
            out.append(vals)
    return out


def _treepos(codepos):
    "offsets from known codes that correspond to the target code"
    offset = -1
    for offset, names in enumerate(reversed(codepos)):
        for name in names:
            yield name, offset
    yield None, offset


def _seq_values(trees, codepos):
    """parse numbered values from the tree and return a list of
       (index, subtree) tuples
    """
    # if sequence starts from beginning of tree the codepos is empty.
    treepos = dict(_treepos(codepos))
    values = {}
    for tree in trees:
        for (code, offset), subtree in tree:
            if code in treepos:
                # -1 means zero'th index is one after the last named code.
                name = offset - treepos[code] - 1
                if name < 0:
                    continue
            else:
                # may be an index or an index + reference
                try:
                    name = int(code) + offset
                except ValueError:
                    continue
            values.setdefault(name, []).extend(subtree)
    return values.items()


def _glob_values(trees, codepos):
    takencodes = set(n for c in codepos for n in c)
    takencodes.add(None)
    vals = {}
    for tree in trees:
        # any (name,0) keys not in codepos are valid
        for (code, offset), subtree in tree:
            if offset == 0 and code not in takencodes:
                vals.setdefault(code, []).extend(subtree)
    return vals.items()


def _get_subtree(trees, codepos):
    """return the list of subtrees/values at this tree point"""
    treepos = set(_treepos(codepos))
    subtree = []
    for tree in trees:
        for pos, v in tree:
            if pos in treepos:
                subtree.extend(v)
    return subtree


# Segment tree building is easier than parsing since it is generating
# a single segment. Wheras parsing requires combining series of
# segments that may be from parameters in the query string.

def _pushtree(utree):
    # Parse only the segments needed at this level. Deeper values are
    # placed in the correct position via the codepath and shouldn't be
    # parsed as they may contain reserved chars from form fields
    tree = []
    for t in utree:
        if isinstance(t, list):
            # split values (already parsed), and ready for pushdown.
            # this includes the raw strings, to prevent them from ever
            # being parsed.
            tree.append((((None,0), [t]),))
        elif isinstance(t, tuple):
            # already parsed tree structure
            tree.append(t)
        else:
            # unparsed, prepared which need to be parsed, if we need
            # to descend
            parsed = _parse_segment(iter(_seg_pat.findall(t)))
            tree.extend(parsed)
            
    return tree


def _nxt(struclist, tree, codepos):
    if isinstance(struclist[0][-1], dict):
        # get the key from the default element
        code, name, disp = struclist[0]
        struclist = struclist[1:]
        codepos.append(set([name, code]))
        subtree = _get_subtree(tree, codepos)
        yield name, None, subtree, []
        vals = _split_vals(subtree)
        # use the default struc when all vals
        if vals:
            s = disp.get(vals[0], struclist)
            for v in vals[1:]:
                if s is not disp.get(v, struclist):
                    break
            else:
                struclist = s

    for code, name, struclist in struclist:
        if code == ".":
            # numerically indexed sequence of keys
            for name, subtree in _seq_values(tree, codepos):
                # must add elem to codepos so seq positions are not
                # mistaken for named value positions
                codepos.append(set()) 
                yield name, struclist, subtree, []
        elif code == "*":
            # glob element matches any remaining named elements
            for name, subtree in _glob_values(tree, codepos):
                yield name, struclist, subtree, [] 
                #codepos.append(set())
        elif code == "+":
            # 'inline' tree element: this parent node contains
            # its elements and the inlined child elements. so also
            # pass the parent for the child and let the struc pick
            # out the required elements.
            yield name, struclist, tree, codepos
        else:
            codepos.append(set([name, code]))
            subtree = _get_subtree(tree, codepos)
            yield name, struclist, subtree, []


def _unpack(struclist, tree, codepos):
    "compute the state from the structure and segment tree"
    if struclist is None:
        return _split_vals(tree)
    tree = _pushtree(tree)
    state = {}
    for name, struclist, subtree, subcodepos in _nxt(struclist, tree, codepos):
        substate = _unpack(struclist, subtree, subcodepos)
        if name == "+":
            state.update(substate)
        elif substate:
            state[name] = substate
    return state


def _seqstate(state):
    "interpret substates as an ordered mapping or integer keys"
    if state is None:
        return ()
    try:
        getitems = state.items
    except AttributeError:
        return enumerate(state)
    return sorted(v for v in getitems() if isinstance(v[0], int))


def _globstate(state, taken):
    getitems = state.items
    return sorted(v for v in getitems()
                  if isinstance(v[0], str) and v[0] not in taken)


def _substate(state, name):
    "interpret the substate indexed by key"
    if name == "+": # this state is also the substate
        return state
    try:
        return state.get(name)
    except AttributeError:
        return


def _nextp(struclist, state):
    # yield elements at this tree level (even if they come from the
    # inlined state), and a flag indicating when code needs to be
    # explicit because the element is not in its natural position.
    show = False
    taken = set()

    if isinstance(struclist[0][2], dict):
        # get the key from the default element
        code, name, disp = struclist[0]
        struclist = struclist[1:]
        substate = _substate(state, name)
        if substate:
            struclist = disp.get(substate[0], struclist)
        taken.add(name)
        yield code, None, substate, show

    for code, name, struclist in struclist:
        if code == ".":
            pos = 0
            for index, substate in _seqstate(state):
                taken.add(index)
                yield str(index), struclist, substate, pos != index
                pos = index + 1
            show = True # indicates that subsequent item is not from seq.
        elif code == "*":
            for gname, substate in _globstate(state, taken):
                #taken.add(gname)

                yield gname, struclist, substate, True
        else:
            taken.add(name)
            yield code, struclist, _substate(state, name), show
            show = False


def _pack(struclist, state, imis=False):
    # generate the tree from the state
    tree = []
    if struclist is None:
        return state, False
    for code, struclist, substate, smis in _nextp(struclist, state):
        if code == "+":
            # get imis from the end of the inlined elements
            subtree, imis = _pack(struclist, substate, imis)
            tree.extend(subtree)
        else:
            subtree = _pack(struclist, substate)[0]
            if subtree: # add matched subtree
                tree.append((code if smis or imis else None, subtree))
                imis = False
            else: # missing subtree
                imis = True
    return tuple(tree), imis


def _build_tokens(tree):
    if not isinstance(tree, tuple):
        if not tree:
            return []
        return [",".join([quote(v, encoding=encoding) for v in tree])]
    tokens = []
    for code, subtree in tree:
        if subtree is None:
            continue
        subtokens = _build_tokens(subtree)
        if len(subtokens) > 1:
            subtokens = ["("] + subtokens + [")"]
        if code is not None:
            subtokens = [quote(code, encoding=encoding), "="] + subtokens
        if tokens and tokens[-1] != ")" and subtokens and subtokens[0] != "(":
            tokens.append(";")
        tokens.extend(subtokens)
    return tokens


def pack(struclist, state):
    """convert state into a single canonical segment"""
    tree = _pack(struclist, state)[0]
    if not isinstance(struclist, list) and isinstance(tree, list):
        # don't quote special chars in string
        tokens = [",".join(tree)] if tree else []
    else:
        tokens = _build_tokens(tree)
    if tokens:
        return "".join(tokens)


def unpack(struc, segments):
    """merge segments into a state"""
    trees = []
    for pathseg in segments:
        codepath = "~"
        if isinstance(pathseg, tuple):
            codepath, pathseg = pathseg # root segment is implied
        tree = [pathseg]
        if codepath != "~":
            for code in reversed(codepath.split(".")):
                tree = [(((code,0), tree),)]
        trees.extend(tree)
    return _unpack(struc, trees, [])


def _split(keypath):
    "parse and split path into a basepath and key or none if path is empty"
    if not isinstance(keypath, (tuple, list)):
        keypath = keypath,
    if not keypath:
        raise ValueError("empty keypath")
    return keypath[:-1], keypath[-1]


class State:
    "make using the state from knurl more convenient for web-app code"
    def __init__(self, state):
        "dict-like constructor"
        self.state = state

    def __getitem__(self, keypath):
        """return a substate instance, may be a value or a dict

        always returns a non-empty list or substate.

        keypath can be None, a tuple or a string
        """
        keypath, key = _split(keypath)
        state = self.state
        for keyname in keypath:
            state = state[keyname]
        state = state[key]
        if isinstance(state, dict):
            return State(state)
        elif isinstance(state, list):
            return state
        else:
            raise RuntimeError("unexpected state type %s" % repr(state))

    def __contains__(self, keypath):
        keypath, key = _split(keypath)
        state = self.state
        for keyname in keypath:
            if keyname not in state:
                return False
            state = state[keyname]
        return key in state

    def __iter__(self):
        return iter(self.state)

    def getstate(self, keypath):
        """like getitem but return an empty state instead of a key error
        and raise an exception if there is a value.
        """
        try:
            result = self.__getitem__(keypath)
        except KeyError:
            return State({})
        if isinstance(result, list):
            raise ValueError("substate is a value %s" % repr(result))
        return result

    def getlist(self, keypath):
        "return the list of values or raise error if not a value"
        try:
            result = self.__getitem__(keypath)
        except KeyError:
            return []
        if not isinstance(result, list):
            raise ValueError("state is not a value %s" % repr(result))
        return result

    def getfirst(self, keypath, default=None):
        "return the first value or raise error if state isn't a value"
        value = self.getlist(keypath)
        return value[0] if value else default


class Structure:
    "the root node. created by a description."
    def __init__(self, struc, smap, name):
        self._struc = struc
        self._smap = smap
        self.name = name

    def __getitem__(self, keypath):
        """return a sub structure, chopping of the keypath"""
        # this allows the sub param to be treated as a unit, e.g. for
        # forms used for variables from at different depths, but
        # decoupling from the absolute path.
        keypath, keyname = _split(keypath)
        smap = self._smap
        codepath = []
        for k in keypath + (keyname,):
            if smap is None:
                raise KeyError("key beyond structure")
            if k == ".":
                raise KeyError(". not allowed")
            if isinstance(k, int):
                key = "."
            elif k not in smap and "*" in smap:
                key = "*"
            else:
                key = k
            cp, smap, struc = smap[key]
            for c in cp:
                codepath.append(str(k) if c in "*." else c)
        if self.name != "~":
            codepath = [self.name] + codepath
        name = ".".join(codepath)
        return Structure(struc, smap, name)

    def value(self, state):
        "get the canonical value string, to use in a URL or form field value"
        return pack(self._struc, state)

    def parse(self, segments):
        "get the state from the list of segments"
        return unpack(self._struc, segments)

    def state(self, segments):
        "return the easy-to-use wrapped state"
        return State(self.parse(segments))


## Public API

def struct(strucdef):
    "initialise from the strucdef string"
    struc, smap = structure(strucdef)
    return Structure(struc, smap, "~")
