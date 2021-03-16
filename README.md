```bash
cabal run ns-exe transpile example.ns
# or
cabal run ns-exe typecheck example.ns
```
## All examples [here](examples/)

## roadmap:
- [x] typecheker
- [ ] typecheker with subtyping
- [ ] warnings about var-patterns shadowing
- [x] polymorphism
- [ ] pattern matching validation
- [ ] type inference for patterns let-in/match
- [x] string manipulations and interpolation
- [ ] do-notation
- [ ] example with type-level parser!

## examples:
### function and constants
```lean
def sum-of-three(a b c : number) : number :=
  plus(a, plus(b, c))

def answer? : number :=
  sum-of-three(10, 20, 12)

def answer-with-let? : number :=
  let (a : number) := 10
      (b : number) := 20
   in sum-of-three(a, b, 12)
```
```typescript
type sum_of_three<a extends number, b extends number, c extends number> = the<number,
  plus<a, plus<b, c>>>

type answer_qmark = the<number,
  sum_of_three<10, 20, 12>>
  
type answer_with_let_qmark = the<number,
  10 extends the<number, (infer a)> ?
  20 extends the<number, (infer b)> ?
  sum_of_three<a, b, 12> : never : never>

```

```lean
def fst(x : tuple(number, string)) : number :=
  match x with | (a, _) := a end

def snd(x : tuple(number, string)) : string :=
  let (_, b) := x in b

def x : tuple(number, string) := (42, "hello")
def y : tuple(string, number) := (snd(x), fst(x))
```
```typescript
type fst<x extends [number, string]> = the<number,
  x extends [(infer a), unknown] ? a : never>

type snd<x extends [number, string]> = the<string,
  x extends [unknown, (infer b)] ? b : never>

type x = the<[number, string], [42, "hello"]>
type y = the<[string, number], [snd<x>, fst<x>]>

```

### inductive types and matching
```lean
enum rgb
| mk-rgb(r g b : number)

enum color
| red
| green
| blue
| mix(v : rgb)

def to-rgb(c : color) : rgb :=
  match c with
  | red := mk-rgb(255, 0, 0)
  | green := mk-rgb(0, 255, 0)
  | blue := mk-rgb(0, 0, 255)
  | mix(rgb) := rgb
  end

def colors : array(rgb) :=
  [ to-rgb(red)
  , to-rgb(blue)
  , to-rgb(mix(mk-rgb(10, 20, 30)))
  ]
```
```typescript
type rgb = the<unknown,
  ["mk-rgb", number, number, number]>
type mk_rgb<r extends number, g extends number, b extends number> = the<rgb,
  ["mk-rgb", r, g, b]>

type color = the<unknown,
  | "red"
  | "green"
  | "blue"
  | ["mix", rgb]>
type red = the<color, "red">
type green = the<color, "green">
type blue = the<color, "blue">
type mix<v extends rgb> = the<color, ["mix", v]>

type to_rgb<c extends color> = the<rgb,
  c extends red ? mk_rgb<255, 0, 0> :
  c extends green ? mk_rgb<0, 255, 0> :
  c extends blue ? mk_rgb<0, 0, 255> :
  c extends mix<(infer rgb)> ? rgb
  : never>

type colors = the<rgb[],
  [to_rgb<red>,
   to_rgb<blue>,
   to_rgb<mix<mk_rgb<10, 20, 30>>>
  ]>

```

```lean
enum tree
| leaf(n : number)
| node(l r : tree)

def sum (x : tree) : number :=
  match x with
  | leaf(5) := 0
  | leaf(x) := x
  | node(leaf(a), leaf(b)) := plus(10, plus(a, b))
  | node(l, r) := plus(sum(l), sum(r))
  end

def q : number := sum(
  node(node(node(leaf(1), leaf(2)), node(leaf(3), leaf(4))), leaf(5))
)
```
```typescript
type tree = the<unknown,
  | ["leaf", number]
  | ["node", tree, tree]>
type leaf<n extends number> = the<tree, ["leaf", n]>
type node<l extends tree, r extends tree> = the<tree, ["node", l, r]>

type sum<x extends tree> = the<number,
  x extends leaf<5> ? 0 :
  x extends leaf<(infer x)> ? x :
  x extends node<leaf<(infer a)>, leaf<(infer b)>> ?
    plus<10, plus<a, b>> :
  x extends node<(infer l), (infer r)> ?
    plus<sum<l>, sum<r>> :
  never>

type q = the<number, sum<
  node<node<node<leaf<1>, leaf<2>>, node<leaf<3>, leaf<4>>>, 
  leaf<5>>>>

```

### externals and declares
```lean
external bool : top := "boolean"
external tt : bool := "true"
external ff : bool := "false"

external not-1(x : bool) : bool :=
  "x extends true ? false : true"

def not-2(x : bool) : bool :=
  match x with
  | .tt := ff
  | _ := tt
  end

def not-ff : bool := not-1(ff)
def not-tt : bool := not-2(tt)
```
```typescript
type bool = the<unknown, boolean>
type tt = the<bool, true>
type ff = the<bool, false>

type not_1<x extends bool> = the<bool,
  x extends true ? false : true>

type not_2<x extends bool> = the<bool,
  x extends tt ? ff :
  x extends unknown ? tt :
  never>

type not_ff = the<bool, not_1<ff>>
type not_tt = the<bool, not_2<tt>>

```

### simple expression language
```lean
external context : top := "Record<string, number>"
external context-empty : context := "{}"
external context-get(ctx : context, key : string) : number := "ctx[key]"
external context-set(ctx : context, key : string, val : number) : context :=
  "(Omit<ctx, key> & Record<key, val>)"

enum expr
| lit(value : number)
| ref(variable : string)
| add(a b : expr)
| local(variable : string, value next : expr)

def eval(ctx : context, e : expr) : number :=
  match e with
  | lit(x) := x
  | ref(x) := context-get(ctx, x)
  | add(a, b) := plus(eval(ctx, a), eval(ctx, b))
  | local(name, value, next) :=
      let (evaled-value : number) := eval(ctx, value)
          new-context := context-set(ctx, name, evaled-value)
       in eval(new-context, next)
  end

def pure(e : expr) : number :=
  eval(context-empty, e)

def a : number :=
  pure(local("a", add(lit(1), lit(20)), add(ref("a"), ref("a"))))
```
```typescript
type context = the<unknown, Record<string, number>>
type context_empty = the<context, {}>
type context_get<ctx extends context, key extends string> = the<number, ctx[key]>
type context_set<ctx extends context, key extends string, val extends number> = the<context,
  (Omit<ctx, key> & Record<key, val>)>

type expr = the<unknown,
  | ["lit", number]
  | ["ref", string]
  | ["add", expr, expr]
  | ["local", string, expr, expr]>
type lit<value extends number> = the<expr, ["lit", value]>
type ref<variable extends string> = the<expr, ["ref", variable]>
type add<a extends expr, b extends expr> = the<expr, ["add", a, b]>
type local<variable extends string, value extends expr, next extends expr> = the<expr, ["local", variable, value, next]>

type eval<ctx extends context, e extends expr> = the<number,
  e extends lit<(infer x)> ? x :
  e extends ref<(infer x)> ? context_get<ctx, x> :
  e extends add<(infer a), (infer b)> ?
    plus<eval<ctx, a>, eval<ctx, b>> :
  e extends local<(infer name), (infer value), (infer next)> ? 
    eval<ctx, value> extends the<number, (infer evaled_value)> ?
    context_set<ctx, name, evaled_value> extends (infer new_context) ? 
    eval<new_context, next> : never : never :
  never>

type pure<e extends expr> = the<number,
  eval<context_empty, e>>
  
type a = the<number, pure<local<"a", add<lit<1>, lit<20>>, add<ref<"a">, ref<"a">>>>>
```