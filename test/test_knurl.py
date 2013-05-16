"tests for knurl"

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
import knurl

def test_subpack_cond():
    st = knurl.struct("g{a, [m], [av]}")
    state = st.parse(["av"])
    assert state == {"g": {"a": ["av"]}}

    # don't define conditional state inside the substate.
    param = st["g", "a"]
    assert param.name == "g.a", param.name
    assert param.value(["av"]) == "av"

def test_inline_param():
    # inline works before
    struct = knurl.struct("+ {a}, b {c}")
    param = struct["b", "c"]
    assert param.name == "b.c"

def test_inline_param2():
    struct = knurl.struct("+{n, [t], q}")
    param = struct["q"]
    assert param.value(["v"]) == "v"
    assert param.name == "q"

def test_path_collision():
    # some structures would map the same keypath to multiple code
    # paths, depending on the state of other vars.
    st = knurl.struct("a, [u], [v]")
    try:
        param = st["b"]
    except KeyError:
        pass
    else:
        raise AssertionError("state should not match")

    param = st["a"]
    assert param.name == "a"

def test_recursive_path():
    st = knurl.struct("label = {a, l {$label}, [x]}, $label")
    param = st["l", "l", "l", "l"]
    assert param.name == "l.l.l.l"
    assert param.value({"a": ["x"]}) == "x"

def test_param_split():
    st = knurl.struct("an a{b {c {d}}}")
    param = st
    assert param.name == "~"
    assert param.value({"an":{"b":{"c":{"d":["x"]}}}}) == "x"

    subparam = param["an"]
    assert subparam.name == "a"
    assert subparam.value({"b":{"c":{"d":["x"]}}}) == "x"

    subparam = param["an", "b"]
    assert subparam.name == "a.b"
    assert subparam.value({"c":{"d":["x"]}}) == "x"

    subsubparam = subparam["c"]
    assert subsubparam.name == "a.b.c"
    assert subsubparam.value({"d":["x"]}) == "x"


def test_deep_state_shallow_path():
    # state may need to go arb. deep beyond keypath, to get a match.
    # that is OK.
    st = knurl.struct("a {b {c, [m]}, v}")
    param = st["a"]
    val = param.value({"b": {"c": ["m"]}, "v": ["val"]})
    assert val == "m;val"
    assert param.name == "a"
    assert st.parse([(param.name, val)]) == {'a':{'b':{'c':['m']},'v':['val']}}


_codepath_error = [
    (".", ("a",), "non-inter key"),
    (".", (".",), "dot not allowed"),
    (".", ("10",), "string-inter key"),
    ("a an", ("x",), "wrong name"),
    ("a", ("a","b"), "too many keys"),
    ("a{b}", ("b",), "wrong sub key"),
    ]

def codepath_error(strucdesc, keypath):
    st = knurl.struct(strucdesc)
    try:
        st[keypath]
    except KeyError:
        return
    raise AssertionError("no KeyError for in valid key (%r)"
                         % repr(keypath))
def test_codepath_error():
    for strucdef, keypath, desc in _codepath_error:
        yield codepath_error, strucdef, keypath

def test_empty_key():
    # test that an empty key is parsable
    uristate = [('~', '=')]
    structure = knurl.struct("a")
    assert structure.parse(uristate) == {}

_codepath_check = [
    (".", (10,), "10", "integer key"),
    ("an a{. {xn x}}", ("an", 10, "xn"), "a.10.x", "random test"),
    ("a{b}", ("a",), "a", "sub key"),
    ("a{b}", ("a","b"), "a.b", "nested key"),
    ]
def codepath_check(strucdef, keypath, result, desc):
    st = knurl.struct(strucdef)
    assert st[keypath].name == result, desc
def test_codepath_check():
    for row in _codepath_check:
        yield (codepath_check,) + row


def test_invalid_structs():
    invalid_structs = [
        "[x]",
        "{a}",
        "+(a{b})",
        "a{b}}",
        "a{[x]}",
        "a:+{",
        "xc(+:a{xx})",# inline inside conditional is meaningles
        "a{",
        "a,,",
        ",",
        "label = {x}, a, $label", # label must be nested
        "label = {x}, $label, a", # label must be nested
        "label = {b}, a, [x], $label", # no label as a condition
        
        # differing key overlaps.
        "a, a {x}",
        "a, [x], bn b, [y], bn c",
        # two keynames on different levels code collision
        "a, [x], bm b, [y], bn b",
        "a, + x {a}",
        "a + {b}, b",
        "a, [x], b, [y], b {q}",
        
        # substructure on conditional key.
        "a {x}, [v], b",

        "n, n",
        "g x, g n",
        "f x, g x",
        "n, [x y z], n",
        "name n, [x y z], n",
        "s n, [x y z], s m",
        "j n, [x y z], k n",
        "n, [x], a, n, [y], n, a",

        "+ {n}, + {n}",

        # duplicated code
        "n b {c}, m + { + b {c}}"


                       ]
    for txt in invalid_structs:
        try:
            knurl.struct(txt)
        except knurl.StructSyntaxError as e:
            pass
        else:
            raise AssertionError("no syntax error for bad struct syntax "
                                 "passed for %s" % txt)


def test_overlap():
    "test the pushdown works for different levels in different trees"
    st = knurl.struct("a{b,c}")
    v = st.parse(["a=(v2;x)", "v1"])
    assert v == {'a': {'c': ['x'], 'b': ['v2', 'v1']}}

    st = knurl.struct("a,b")
    v = st.parse(["a=v1;vx", "b=v2"])
    assert v == {'a': ['v1'], 'b': ['vx', 'v2']}

    st = knurl.struct(".")
    v = st.parse(["a1;a2", "b1;b2"])
    assert v == {0: ['a1', 'b1'], 1: ['a2', 'b2']}

    st = knurl.struct(".")
    assert st.parse([";"]) == {0: [''], 1: ['']}
    assert st.parse([";;"]) == {0: [''], 1: [''], 2: ['']}
    assert st.parse([""]) == {0: ['']}


def test_conditional_positions():
    # duplicated names in different positions are allowed
    st = knurl.struct("n, [x], b, [y], a, b")

    assert st.parse([]) == {}
    # use default structure when key names are different
    assert st.parse(["x;b1", "y;a;b2", "z"]) == {"n": ["x", "y", "z"]}


def test_nonlist():
    "can build conditional match from strings"
    st = knurl.struct("p +{n, [tr], o}")
    state = {"p":{"n":["tr"], "o":["v"]}}
    assert st.value(state) == "tr;v"

    
def eqv(struc1, struc2):
    assert knurl.structure(struc1) == knurl.structure(struc2)


def test_equiv_structures():
    eqv("  ", "") # space is empty
    eqv("a # a comment\n#", "a") # trailling comments allowed
    #eqv("a{b,|},|" , "a{b}") # single trailing operators are allowed


def test_canonical_substate():
    """check that the canonical is correct, when non-empty substate exists
    although the actual key is missing.
    """
    st = knurl.struct("+ {a {missing}}, b")
    v1 = st.value({"b": ["x"], "a": {None: None}})
    v2 = st.value({"b": ["x"]})
    assert v1 == v2


def test_glob_param():
    struct = knurl.struct("*{a}")
    param = struct

    sp = param["q"]
    assert sp.name == "q"
    assert sp.value({"a": ["x", "y"]}) == "x,y"

    ssp = sp["a"]
    assert ssp.name == "q.a"
    assert ssp.value(["z"]) == "z"

    assert param["x", "a"].name == "x.a"

    try:
        sp["q", "p"]
    except KeyError:
        pass
    else:
        raise AssertionError("should raise Match Err")



# Ideas:
#  - raise Error if a value is specified as a string instead of a list.
#  - allow lists with None elements for missing values. 


_tests = [
    dict(desc="empty structure doesn't match empty segment",
         structure="",
         segment=[],
         state=[]),

    dict(desc="empty struclist should not match anything",
         structure="",
         segment=["x"],
         state=["x"]),

    dict(desc="one variable",
         structure="key",
         segment=["value"],
         state={"key": ["value"]}),

    dict(desc="inline match",
         structure="an +{bn b, [v]}",
         segment=["v"],
         state={"an": {"bn": ["v"]}}),


    dict(desc="canonical too long",
         structure="a{.{o, q}}, g",
         segment=["o1;gv"],
         state={'a': {0: {'o': ['o1']}}, 'g': ['gv']}),

    dict(desc="single trailing operator allowed in structure def",
         structure="n," ,
         segment=["v"],
         state={"n": ["v"]}),

    dict(desc="when no strings (even empty) canonical is None",
         structure="s",
         segment=[],
         state={}),

    dict(desc="no segments with list structure",
         structure=".",
         segment=[],
         state={}),

    dict(desc="values should not be quoted",
         structure="a",
         segment=[("a", "v;c);(")],
         state={"a": ["v;c);("]}),

    dict(desc="deep values should not be quoted",
         structure="a{b{c}}",
         segment=[("a.b.c", "(")],
         state={"a":{"b":{"c":["("]}}}),

    dict(desc="value quoted when codepath is duplicated, with non-value",
         structure="a",
         segment=[("a", ";")], 
         state={"a": [";"]}),

    dict(desc="mismatch of a deeper first structure with same name",
         structure="a, [x]",
         segment=["v"], 
         state={"a": ["v"]}),

    dict(desc="handle unquoted unicode chars in segment",
         structure="a",
         segment=["\xe9"],
         state={"a": ["\xe9"]}),

    dict(desc="handle quoted unicode chars",
         structure="a",
         segment=['%C3%A9'],
         state={"a": ["\xe9"]}),

    dict(desc="unicode chars in codepath",
         structure="\xe9",
         segment=["\xe9=x", "%C3%A9=x"],
         state={"\xe9": ["x", "x"]}),

    dict(desc="generate quoted codepaths",
         structure="v, \xe9",
         canonicals=["%C3%A9=x"],),

    dict(desc="test overlapping segments",
         structure="a",
         segment=["a", "b"],
         state={"a":["a", "b"]}),

    dict(desc="test parsed value exceeds structure position",
         structure="a",
         segment=["a;b;c"],
         state={"a":["a"]}),

    dict(desc="test parsed value exceeds structure depth",
         structure="a",
         segment=["((a;b;c))"],
         state={}),

    # structures with multiple fields
    
    dict(desc="three named values",
         structure="a,b,c",
         segment=["av;bv;cv"],
         state={'a': ['av'], 'c': ['cv'], 'b': ['bv']}),

    dict(desc="simple test",
         structure="an a, bn b",
         segment=["av"],
         state={"an":["av"]}),

    dict(desc="unordered1",
         structure="a,b,c",
         segment=["c=a1;a=a2;b=a3;c=v"],
         state={"a":["a2"], "b":["a3"], "c":["a1","v"]}),

    dict(desc="test named canonical is shortert than segment",
         structure="a,b,c,d",
         state={"b":["b"],"c":["c"]},
         segment=["b=b;c"]),

    dict(desc="multiple unordered names",
         structure="a,b,c",
         segment=["b=b1;a=a1;b2;c1;b=b3"],
         state={"a":["a1"], "b":["b1","b2","b3"], "c":["c1"]}),

    dict(desc="keys exist but values are None when segment is empty string",
         structure="a,b",
         segment=[""],
         state={"a":['']}),

    dict(desc="empty string using empty parens in segment",
         structure=".{.}",
         segment=["()()"],
         state={0: {0:[""]}, 1: {0:[""]}}),

    dict(desc="named assignment to empty string 1",
         structure="a,b",
         segment=["b="],
         state={"b":[""]}),

    dict(desc="named assignment to empty string 2",
         structure="a,b",
         segment=["a;b="],
         state={"a":["a"], "b":[""]}),

    dict(desc="multiple values for single segmnent",
         structure="a,b",
         segment=["x,y,z;g"],
         state={"a":["x","y","z"], "b":["g"]}),

    dict(desc="tree with 2 named",
         structure="x,y",
         segment=["x=a;y1;x=b;y2"],
         state={"x":["a","b"], "y":["y1","y2"]}),
         
    # dot notation for sequences

    dict(desc="basic sequence 1",
         structure=".",
         segment=["av;bv;cv"],
         state={0: ['av'], 1: ['bv'], 2: ['cv']}),

    dict(desc="unknown code after sequence",
         structure=".",
         segment=["x;y=y"],
         state={0: ["x"]}),

    dict(desc="unknown code for sequence",
         structure=".",
         segment=["a=av"],
         state={}),

    dict(desc="sequence indices are preserved",
         structure=".",
         state={5:["v5"], 2:["v2"]},
         segment=["2=v2;5=v5"]),

    dict(desc="test indexed canonical is shortert than segment",
         structure=".",
         state={1:["b"],2:["c"]},
         segment=["1=b;c"]),

    dict(desc="named followed by sequence",
         structure="a,b,.",
         segment=["a1;a2;v1;v2"],
         state={"a": ["a1"], "b": ["a2"], 0: ["v1"], 1: ["v2"]}),

    dict(desc="sequence consumes elements before a name, but add key for name",
         structure="., b",
         segment=["x;y"],
         state={0: ["x"], 1: ["y"]}),

    dict(desc="names after sequence",
         structure=".,b",
         segment=["b=b"],
         state={"b":["b"]}),

    dict(desc="sequence between two names",
         structure="a,.,b",
         segment=["a;0;1;b=b"],
         state={"a": ["a"], 0:["0"], 1:["1"], "b":["b"]}),

    dict(desc="sequence consumes inlined elements before a missing name",
         structure="n +{.}, z",
         segment=["x;y"],
         state={"n": {0: ["x"], 1: ["y"]}}),

    # braces for nested structures

    dict(desc="named inside named",
         structure="a {b}",
         segment=["(v)"],
         state={"a": {"b": ["v"]}}),

    dict(desc="sequence inside name",
         structure="a {.}",
         segment=["(v1;v2;v3)"],
         state={"a": {0: ["v1"], 1: ["v2"], 2: ["v3"]}}),

    dict(desc="sequence inside sequence",
         structure=". {.}",
         segment=["(a;b)(c;d)"],
         state={0:{0: ["a"], 1:["b"]}, 1: {0: ["c"], 1:["d"]}}),

    dict(desc="list containing dict with single value",
         structure=". {x}",
         segment=["(a)(b)"],
         state={0: {'x': ['a']}, 1: {'x': ['b']}}),

    dict(desc="sequence out of order and value after numbered without paren",
         structure=".{x}",
         segment=["5=z;2=(a)b"],
         state={5:{"x":["z"]}, 2:{"x": ["a"]}, 3:{"x":["b"]}}),

    dict(desc="mix integers and letters",
         structure="a,b,.",
         segment=[("3", "3"), "a=a;1=1;b=b;0=0;2=2"],
         state={"a":["a"], "b": ["b"], 0:["0"], 1:["1"], 2:["2"], 3:["3"]}),

    dict(desc="integers starting at explicit index zero",
         structure="a,b,.",
         segment=["0=0v"],
         state={0:["0v"]}),

    dict(desc="missing structure after first value",
         structure="an a, bn b{. {cn c}}",
         segment=["v"],
         state={'an': ['v']}),

    dict(desc="non-braced list",
         structure="z {. {a, b}}",
         segment=["((0a;0b)1a)"],
         state={"z": {0:{"a": ['0a'],"b":["0b"]},1:{"a": ["1a"]}}}),

    dict(desc="no empty dict in generated state",
         structure="a {.}, x",
         segment=["x=y"],
         state={"x": ["y"]}),

    dict(desc="test more than one val inlined",
         structure="a, b +{c {x, y}}",
         segment=["av(x1;y1)"],
         state={"a": ["av"], 'b': {'c': {'y': ['y1'], 'x': ['x1']}}}),

    dict(desc="inline indices should wourk for missing values",
         structure="a +{x, y}, b",
         state = {'a': {'x': ["xv"]}, "b": ["bv"]}),

    dict(desc="test codepaths",
         structure="a{b{c,d,.}}",
         segment=["v1", ("a.b.d", "v2"), ("a.b", "7=v3;v4"),
                  ("a.b.3", "v5")],
         state={'a':{'b':{'c':['v1'],'d':['v2'],3:["v5"],7:['v3'],8:['v4']}}}),

    dict(desc="missing name between other keys",
         structure="bn b, mn m, an a{. {on o}}",
         state={'an': {0: {'on': ['ov']}}, 'bn': ['bv']}),
         
    dict(desc="test whether can accept lists",
         structure=". {on o, bv b}",
         state={0:{'on': ['0v']}, 1:{'on': ['1v']}}),

    dict(desc="named numerical keys",
         structure="1,2",
         segment=["1v;2v"],
         state={"1":["1v"], "2": ["2v"]}),

    dict(desc="test list of values",
         structure="lic{.}",
         segment=[("lic.0", "a"), ("lic.0", "b"), ("lic.0", "a")],
         state={"lic": {0: ["a", "b", "a"]}}),

    dict(desc="test double deep tree",
         structure=". {. {.}}",
         segment=["((x))"],
         state={0: {0: {0: ["x"]}}}),

    dict(desc="commas in a value don't get split when in a form",
         structure="a",
         segment=[("a", "x,y")],
         state={"a": ["x,y"]}),

    dict(desc="test slashes are interpreted",
         structure="a",
         segment=[("~", "a/b,a%2Fb")],
         state={"a": ["a/b", "a/b"]}),

    dict(desc="test url encoding",
         structure=".{a,b}",
         state={5:{"a": ["normal/+5"]},
                2:{"a": ["v!1"], "b": ["b =(1)"]}},
         segment=["2=(v%211;b%20%3D%281%29)5=normal%2F%2B5"]),

    dict(desc="broke canonical len check",
         structure="s, a{.{o}}",
         segment=["a=(x;y)"],
         state={"a": {0: {'o': ['x']}, 1: {'o': ['y']}}}),

    # keypaths for nested structures
    dict(desc="subpack 1",
         structure="an a{b bn{c cn{d dn}}, z}",
         segment=[("a.z", "zv")],
         substate=['zv'],
         keypath=("an","z")),

    dict(desc="subpack 2",
         structure="a{b}, c{b}",
         segment=[("c.b", "val,v")],
         substate=["val", "v"],
         keypath=("c","b")),

    # inlined syntax

    dict(desc="inline named expand",
         structure="b + {c}",
         state={"b": {"c": ["cv"]}},
         segment=["cv"]),

    dict(desc="inlined followed by value",
         structure="c + {c3}, v",
         state={"c": {"c3":["p3"]}, "v":["val"]},
         segment=["p3;val"]),

    dict(desc="inline list expand",
         structure="a, d + {.}",
         state={"a":["a1"], "d": {0: ["0v"], 1: ["1v"]}},
         segment=["a1;0v;1v"]),

    dict(desc="double inline expand",
         structure="a + {b + {.}}",
         state={"a": {"b": {0: ["v"]}}},
         segment=["v"]),

    dict(desc="inline expand 2",
         structure="a, b + {c, d + {.}}",
         state={"a": ["av"], "b": { "c": ["cv"],
                                  "d": {0: ["0v"], 1: ["1v"], 2: ["2v"]}}},
         segment=["av;cv;0v;1v;2v"]),

    dict(desc="single named match",
         structure="a, [match]",
         segment=["match"],
         state={"a": ["match"]}),

    dict(desc="simple catchall",
         structure="a, [b]",
         segment=["v"],
         state={"a": ["v"]}),

    dict(desc="simple named match after mismatch",
         structure="a, [mismatch], [match]",
         segment=["match"],
         state={"a": ["match"]}),

    dict(desc="simple match 2",
         structure="a, [v], b",
         segment=["v;arg"],
         state={"a": ["v"], "b": ["arg"]}),

    dict(desc="simple match 3",
         structure="a, [m], [v], b",
         segment=["v;arg"],
         state={"a": ["v"], "b": ["arg"]}),

    dict(desc="conditional matching inside a list of elements",
         structure="a, b +{c, [cv], d +{.}}",
         segment=["av;cv;xv;yv"],
         state={"a":["av"], "b":{"c":["cv"], "d":{0: ["xv"], 1: ["yv"]}}}),

    dict(desc="position of conditional param when not in position",
         structure="a, c +{b, [d], v}, d",
         state={"c":{"b":["d"],"v":["x"]}},
         segment=["b=d;x"]),

    dict(desc="samename 2",
         structure="a{n, [m1], x1, y1, [m2], x2, y2}",
         canonicals=["(m1;v1)", "(m2;v2)", "(m1;y1=v1)", "(m2;y2=v2)"]),

    dict(desc="API inlined",
         structure="+ a {b}",
         segment=["x"],
         state={"b": ["x"]}),

    dict(desc="full inline",
         structure="+{a}",
         segment=["x"],
         state={"a": ["x"]}),

    dict(desc="2 element catchall, with failing match",
         structure="a, b, [v], q",
         segment=["a;b"],
         state={"a": ["a"], "b": ["b"]}),

    dict(desc="inlined after normal element",
         structure="a, x + {u}",
         segment=["a;m"],
         state={"a": ["a"], "x": {"u": ["m"]}}),

    dict(desc="inlined conditional after normal element",
         structure="a, x +{u, [x], [m]}",
         segment=["a;m"],
         state={"a": ["a"], "x": {"u": ["m"]}}),

    dict(desc="grouped inlined conditionals",
         structure="+{a, [x], f}, + {u, [p], t, [m]}",
         segment=["x;y;m"],
         state={"a": ["x"], "f": ["y"], "u": ["m"]}),

    dict(desc="incorrect state",
         structure="a +{n, [u], [v], j}, b, c",
         segment=["v;jv;bv"],
         state={"a":{"n": ["v"], "j": ["jv"]}, "b": ["bv"]}),
    
    dict(desc="simple structure substitution",
         structure="L = {b}, a {$L} # equiv to a{b}",
         segment=["x"],
         state={"a": {"b": ["x"]}}),

    dict(desc="substitute same definition multiple times",
         structure="l = {.}, a {$l}, b {$l}, c {$l}",
         segment=["x;y;z"],
         state={"a": {0: ["x"]}, "b": {0: ["y"]}, "c": {0: ["z"]}}),

    # note the top level label needs inlining with '+' due to the
    # [label] syntax having implicit braces. maybe look for a more
    # obvious way
    dict(desc="recursive structure definition",
         structure="label = {op, [and], . {$label}}, $label",
         segment=["and(and;1a;1b)(and(and;2a;2b)2c)2d"],
         state={0: {0: {"op": ["1a"]}, 1: {"op": ["1b"]},
                    "op": ["and"]},
                1: {0: {0: {"op": ["2a"]}, 1: {"op":["2b"]},
                        "op": ["and"]},
                    1: {"op":["2c"]},
                    "op": ["and"]},
                2: {"op": ["2d"]},
                "op": ["and"]}),

    dict(desc="allow the keyname to be used in the url",
         # this is more user-friendly e.g. like long commandline
         # options.  and required to avoid ambiguity with globbing.
         structure="an a",
         segment=["an=x", ("an", "y"), ("a", "z")],
         state={"an": ["x", "y", "z"]}),

    dict(desc="basic glob structure",
         structure="*",
         segment=[("~", "a=x"), ("b", "y")],
         state={"a":["x"], "b":["y"]}),

    dict(desc="substruc globbing",
         structure="*{q}",
         segment=["b=(q=y);a=x"],
         state={"a": {"q":["x"]}, "b": {"q": ["y"]}}),

    # note we need an explict path for nested structures so it know
    # wihch fields to parse and which to leave as values

    # TODO: impose parse checking rules:

    # glob must be last element in list.
    # one glob per structure.
    dict(desc="nested globbing",
         structure="name{*}, *",
         segment=[("name.x", "xv"), ("name.y", "yv"), ("z", "zv")],
         state={"name": {"x": ["xv"], "y": ["yv"]}, "z": ["zv"]}),

    dict(desc="nested globbing 2",
         structure="z, *{*}",
         segment=[("name.x", "xv"), ("name.y", "yv"), ("z", "zv")],
         state={"name": {"x": ["xv"], "y": ["yv"]}, "z": ["zv"]}),
         
]


def _gentests(testfunc, *keys):
    for i, case in enumerate(_tests):
        if set(keys) - set(case):
            continue
        def test():
            return testfunc(*tuple(case[k] for k in keys))
        test.description = "%d. %s" % (i, case.get("desc", "(no desc)"))
        yield test

def test_keys():
    allowed = set(["desc", "structure", "segment", "state", "keypath",
                   "overlay", "keypath", "substate", "canonicals",
                   "parse_error"])
    for i, case in enumerate(_tests):
        extra = set(case.keys()) - allowed
        assert not extra, "extra keys"

def test_parse():
    # check that parsed segment is same as the reference state
    def test(structure, segment, state):
        st = knurl.struct(structure)
        parsed = st.parse(segment)
        assert state == parsed
    # why won't nose do the test if the generator is returned?
    for func in _gentests(test, "structure", "segment", "state"):
        yield func

def test_invarient():
    # check that the state is invarient under building, then parsing
    def test(structure, state):
        st = knurl.struct(structure)
        canonical = st.value(state)
        parsed = st.parse([canonical] if canonical is not None else [])
        assert state == parsed
    for test in _gentests(test, "structure", "state"):
        yield test


def test_length():
    # canonical length is <= length of segment
    def test(structure, segment, state):
        st = knurl.struct(structure)
        canonical = st.value(state)
        parsed = st.parse([canonical] if canonical is not None else [])
        assert state == parsed

        if canonical is None:
            return
        # estimate effective segment length:
        effs = []
        for v in segment:
            if effs:
                effs.append(";")
            if isinstance(v, tuple):
                key, val = v
                if key is None:
                    effs.append(val)
                else:
                    effs.append(key)
                    effs.append("=")
                    effs.append(val)
            else:
                effs.append(v)
        efflen = len("".join(effs))

        #unquote since segment values can contain unquoted specials like ";()"
        canstr = knurl.unquote(canonical, encoding=knurl.encoding)
        canlen = len(canstr)
        assert canlen <= efflen , (canstr, effs)
    for test in _gentests(test, "structure", "segment", "state"):
        yield test

def test_keypath():
    def test(structure, segment, state, keypath):
        st = knurl.struct(structure)
        param = st[keypath]
        seg = [(param.name, param.value(state))]
        assert seg == segment
    for test in _gentests(test, "structure", "segment", "substate", "keypath"):
        yield test
    
def test_canonicals():
    def test(structure, canonicals):
        st = knurl.struct(structure)
        for canonical in canonicals:
            parsed = st.parse([canonical])
            built = st.value(parsed)
            assert canonical == built
    for func in _gentests(test, "structure", "canonicals"):
        yield func
        
def test_parse_error():
    def test(structure, segment, exception):
        st = knurl.struct(structure)
        try:
            st.parse(segment)
        except exception:
            pass
        else:
            raise AssertionError(exception)
    for test in _gentests(test, "structure", "segment", "parse_error"):
        yield test
