```bash
cabal run ns-exe transpile example.ns
# or
cabal run ns-exe typecheck example.ns
```

## examples:
### function and constants
```lean
def sum-of-three(a b c : number) : number =>
  plus(a, plus(b, c))

def answer? : number =>
  sum-of-three(10, 20, 12)

def answer-with-let? : number =>
  let a : number => 10
      b : number => 20
   in sum-of-three(a, b, 12)
```
```typescript
type sum_of_three<a extends number, b extends number, c extends number> =
  the<number, plus<a, plus<b, c>>>

type answer_qmark = the<number, sum_of_three<10, 20, 12>>

type answer_with_let_qmark = the<number,
  10 extends the<number, infer a> ?
    20 extends the<number, infer b> ?
      sum_of_three<a, b, 12>
    : never
  : never>
```

### inductive types and matching
```lean
inductive rgb
| mk-rgb(r g b : number)

inductive color
| red
| green
| blue
| mix(rgb : rgb)

def to-rgb(c : color) : rgb =>
  match c
  | red => rgb(255, 0, 0)
  | green => rgb(0, 255, 0)
  | blue => rgb(0, 0, 255)
  | mix(?rgb) => rgb

def colors : array(rgb) =>
  [ to-rgb(red)
  , to-rgb(blue)
  , to-rgb(mix(mk-rgb(10, 20, 30)))
  ]
```
```typescript
type rgb = the<unknown,
  | {"tag": "mk-rgb", "values": {"r": number, "g": number, "b": number}}
  | never>
type mk_rgb<r extends number, g extends number, b extends number> =
  the<rgb, {"tag": "mk-rgb", "values": {"r": r, "g": g, "b": b}}>

type color = the<unknown,
  | {"tag": "red", "values": {}}
  | {"tag": "green", "values": {}}
  | {"tag": "blue", "values": {}}
  | {"tag": "mix", "values": {"rgb": rgb}}
  | never>
type red = the<color, {"tag": "red", "values": {}}>
type green = the<color, {"tag": "green", "values": {}}>
type blue = the<color, {"tag": "blue", "values": {}}>
type mix<rgb extends rgb> = the<color, {"tag": "mix", "values": {"rgb": rgb}}>

type to_rgb<c extends color> = the<rgb,
  c extends red ? mk_rgb<255, 0, 0> :
  c extends green ? mk_rgb<0, 255, 0> :
  c extends blue ? mk_rgb<0, 0, 255> :
  c extends mix<infer rgb> ? rgb :
  never>

type colors = the<rgb[], [
  to_rgb<red>,
  to_rgb<blue>,
  to_rgb<mix<mk_rgb<10, 20, 30>>>
]>
```

### externals
```lean
external bool : U => "boolean"
external tt : bool => "true"
external ff : bool => "false"

external not(x : bool) : bool => "x extends true ? false : true"
def not-ff : bool => not(ff)

def not2(x : bool) : bool
  match x
  | tt => ff
  | ff => tt

def not-tt : bool => not2(tt)
```
```typescript
type bool = the<unknown, boolean>
type tt = the<bool, true>
type ff = the<bool, false>

type not<x extends bool> = the<bool, x extends true ? false : true>
type not_ff = the<bool, not<ff>>

type not2<x extends bool> = the<bool,
  x extends tt ? ff :
  x extends ff ? tt :
  never>
type not_tt = the<bool, not2<tt>>
```