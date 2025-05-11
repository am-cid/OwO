# OwO
compiler for [UwU++](https://github.com/Gidsss/UwUIDE) written in rust.
Language Design is overhauled to have support for optional types, immutable variables by default, sets, maps, assertions, and more!

# Contents
- [Hello World](#hello-world)
- [IO](#io)
- [Declaration and Assignment](#declaration-and-assignment)
- [Function Declaration and Calling](#function-declaration-and-calling)
- [Variadic Parameters](#variadic-parameters)<br><br>
- [Type System](#type-system)
    - [Primitive Types](#primitive-types)
    - [Optional Type](#optional-type)
    - [Collection Types](#collection-types)<br><br>
- [User Defined Types](#user-defined-types)
    - [Groups](#groups)
    - [Contracts](#contracts)<br><br>
- [Control Flow](#control-flow)
    - [If-Else](#if-else)
    - [Mash](#mash)
    - [Loops](#loops)
        - [For Loop](#for-loop)
        - [For Each](#for-each)<br><br>
- [Assertions](#assertions)
- [Function Pipeline](#function-pipeline)
---

# Hello World
All programs must have a main function that takes no arguments and return
nothing nothing (aka return type `san`)
```kotlin
>_< this is a comment

fun main-san() {
    pwint("hello", "world")~
}
```
output: `hello world`
# IO
`pwint` takes a variable amount of arguments of any type. Args are separated by
space when printed

`inpwt` takes one argument only of any type, and will return a string value.
Arg will be printed to the console as the prompt, and will read user input
until the user presses enter.

```kotlin
>_< this is a comment

fun main-san() {
    hi name-senpai = inpwt("what is your first name?: ")~
    pwint("hello", name, "\n")~

    >_< inpwt returns string so it must be converted
    hi age-chan = inpwt("what is your age?: ").to_chan().unwrap()~
    pwint(surname, "was my age a few weeks ago!\n")~

    inpwt("Press Enter to exit...")~
}
```
output:
```
what is your name?: owo
hello owo

what is your last name?: 1000
1000 was my age a few weeks ago!

Press Enter to exit...
```
# Declaration and Assignment
Declare a variable in the format:
```kotlin
hi <variable-name>-<variable-type> = <value-assigned>~
hi aqua-chan = 1~
```
The dash followed by the variable type is immutable by default. To declare a
mutable variable, put a `!` after the name or the type, depending on whether the
type is declared.
```kotlin
>_< both have type chan
hi aqua-chan! = 10~
hi shion! = 10~
```
Example:
```kotlin
>_< declare mutable variables by putting ! after the type
hi global-chan! = 99~ 

fun main-san() {
    >_< declarations are immutable by default
    >_< the type is inferred as chan
    hi num = 1~ 

    >_< reassigning global variable
    global = 0~
    pwint(global, num, global_2)~
}

hi global_2-chan = 2~
```
output: `0 1 2`
# Function Declaration and Calling
Call a function by referencing its name and putting the arguments enclosed in
parenthesis:
```kotlin
<function-name>()
<function-name>(<arg1>, <arg2>)

inpwt()~
pwint("hewwo", "world", ":3")~
```
Example:
```kotlin
>_< this is a comment

fun main-san() {
    pwint(sum(2,3))~
}

>_< args: immutable int and mutable int
>_< returns: int
fun sum-chan(left-chan, right-chan!) {
    >_< change value of mutable int
    right += 1~
    wetuwn left + right~
}
```
output: `5`
# Variadic Parameters
- Functions can take in any number of arguments given a **variadic parameter**
is declared as the last parameter to a function/method.
- Add `...` after the type: `names-senpai...`
- The variadic parameter is equivalent to a vector one dimension higher than the
declared type
    - `names-senpai...` == `names-senpai[1]`
    - `names-senpai[1]...` == `names-senpai[2]`
    - `names-senpai{dono}...` == `names-senpai{dono}[1]`
- The main difference is when calling the function/method
    - `names-senpai...`: `hello_to("aqua", "shion", "ojou")~`
    - `names-senpai[1]`: `hello_to(["aqua", "shion", "ojou"])~`
```kotlin
>_< builtin fowmat function
fun fowmat-senpai(string-senpai, vars-Stringable...) {
    fow var in vars {
        string.replace("{}", var.string())~
    }
    wetuwn string~
}

fun main-san() {
    hi world-senpai = "world"~
    pwint(
        fowmat(
            "{}, {}!",
            "hello",
            world,
        )
    )~
}
```
To declare a variadic parameter of optional types, add `?` before the ellipsis.
```kotlin
fun square_then_print-san(args-Stringable?...) {}
>_< args is Stringable?[1]
```
To declare an optional variadic parameter, add `?` after the ellipsis.
```kotlin
fun square_then_print-san(args-Stringable...?) {}
>_< args is Stringable[1]?
```
You can even get wacky like this:
```kotlin
fun square_then_print-san(args-Stringable?...?) {}
>_< args is Stringable?[1]?
```
output: `hello, world!`

# Type System
## Primitive Types
1. `chan`: integer<br>
    declaration: `hi aqua-chan = 1~`<br>
<br>

2. `kun`: float<br>
    declaration: `hi shion-kun = 1.0~`<br>
<br>

3. `senpai`: string<br>
    declaration: `hi ojou-senpai = "hi"~`<br>
    `senpai` literals are enclosed in `"`<br>
    - indexing into a `senpai` value returns a `kouhai` value
    ```
    hi aqua-senpai = "hello"~
    hi pe-kouhai = aqua[1]~
    ```
<br>

4. `kouhai`: char<br>
    declaration: `hi pe-kouhai = 'o'~`<br>
    `kouhai` literals are enclosed in `'` and can only contain one character<br>
<br>

5. `sama`: boolean<br>
    declaration: `hi lap-sama = fax~` (True)<br>
    declaration: `hi lap-sama = cap~` (False)<br>
<br>

5. `san`: null<br>
    Used for functions that return `nuww` (null) or never returns.
    Identifiers that have this type can only have the value `nuww`.<br>
    function declaration: `fun main-san() { ... }`<br>
    declaration: `hi aqua-san = nuww~`<br>
<br>

6. `dono`: any<br>
    `hi aqua-dono = nuww~`<br>
    `hi shion-dono = 1~`<br>
    `hi ojou-dono = "hello"~`<br>
    - `dono` can be downcasted to any other type using a [`mash`](#mash) statement
    - `dono` values cannot be used like other types (eg. adding a `dono` value to a `chan` value)
    - `dono` alone means any type, even [optional types](#optional-type) and [collection types](#collection-types)

## Optional Type
Declare an optional type by putting a `?` after the type.
```kotlin
>_< valid. optional type chan
hi aqua-chan? = nuww~
>_< invalid, must have a type specified
hi shion? = 10~
```
You need to declare the type if you're planning to use an optional type.
To declare a mutable variable, put a `!` after the name or type, depending on
whether the type is declared. That means, to declare a mutable optional type,
put `!` after `?`.
```kotlin
>_< valid. mutable variable with optional type chan
hi aqua-chan?! = nuww~
>_< invalid, must have a type specified
hi shion?! = 10~
```
While `hi shion? = 10~` and `hi shion?! = 10~` is invalid since the type needs
to be declared when using an optional type, `hi shion! = 10~` is valid. This is
because mutability is not part of the type but is associated with the variable
name.

Example:
```kotlin
>_< optional chan can have an int or nuww as a value
hi aqua-chan? = 1~
hi shion-chan? = nuww~

>_< adding ! after ? turns it into an optional variable that is mutable
hi ojou-senpai?! = "hello"~
ojou = nuww~ 

>_< this will cause an error since the current value is nuww
ojou.unwrap()

>_< you cannot use an optional variable chan as a normal chan
pwint(ojou + shion + aqua)~ >_< will cause an error

>_< use unwrap_or to provide a fallback
assewt(aqua.unwrap_or(0) + shion.unwrap_or(0) == 1)~
assewt(ojou.unwrap_or(0) == 0)~
```

## Collection Types
1. **vector**

    Defined as `type[dim]` where `type` is any type and `dim` is an integer
    literal. Note that `dim` cannot be an identifier. Vector literals must be
    enclosed in square brackets: `[item]`. Other collection types can be vector
    items.

    To get an item from a vector, use:
    1. `.nth()` method
        - `aqua.nth(1)` gets the first item.
        - `aqua.nth(1, 2)` gets the second item (inner) of the first item (outer)
        - returns an optional type
    2. subscript operator `[]`
        - `aqua[1]` gets the first item (equivalent to `aqua.nth(1).unwrap()`)
        - `aqua[1, 2]` is equivalent to `aqua.nth(1, 2).unwrap()`
    ```kotlin
    hi aqua-chan[1] = [1, 2, 3]~
    hi shion-chan[2] = [[1,2], [3,4], [5,6]]~

    assewt(aqua.nth(1).unwrap() == 1)~
    assewt(shion[1, 2] == 2)~
    assewt(shion[1, 2] == shion.nth(1, 2).unwrap())~

    pwint(aqua)~
    ```
    output
    ```
    [1, 2, 3]
    ```

2. **hashset**

    Defined as `type{}` where `type` is any builtin owo type or any type that
    implements the `Hashable` contract. Hashset literals must be enclosed in
    brackets starting with hash: `#[item]`. Empty hashset literals are written
    as: `#[]`. A hashset ensures only unique items are in the hashset even if
    user put in duplicates. You cannot put other collection types into a
    hashset.

    You cannot get items from a hashset.
    ```kotlin
    hi aqua-senpai{} = #[ "1", "1", "2", "3" ]~
    >_< empty set
    hi shion-chan{} = #[]~
    >_< 1D vector of hashsets of floats
    ojou-kun{}[1] = [
        #[1.0, 1.0],
        #[2.0, 3.0],
    ]~
    >_< while kun{}[1] is a valid type, kun[1]{} is not
    >_< since collections cannot be put into hashsets

    pwint(aqua, "\n", shion, "\n", ojou)~
    ```
    output
    ```
    #["1", "2", "3"]
    #[]
    [#[1.0], #[2.0, 3.0]]
    ```

3. **hashmap**

    Defined as `key{val}` where `val` is any type. `key` is any builtin owo
    type or any type that implements the `Hashable` contract. Hashmap literals
    must be enclosed in brackets starting with hash with key and value
    separated by colon `#[key:val]`. Empty hashmap literals are written as:
    `#[:]`. Colon signifies its a hashmap, not a hashset. You cannot put
    collection types as keys.

    To get an item from a hashmap, use:
    1. `.get()` method
        - `aqua.get("item")` gets the value associated with the key `"item"`
        - returns an optional type
    2. subscript operator `[]`
        - `aqua["item"]` is equivalent to `aqua.get("item").unwrap()`
    ```kotlin
    ojou-senpai{chan} = #["id": 1, "age": 18 ]~
    assewt(ojou.get("age").unwrap() == 18)~

    >_< 1D vector of hashmaps of chan:kun
    peko-chan{kun}[1] = [
        #[1: 2.0, 2: 1.0],
        #[9: 5.0, 5: 9.0],
    ]~
    >_< while chan{kun}[1] is a valid type, chan[1]{kun} is not
    >_< since collections cannot be keys for hashmaps
    >_< chan{kun[1]} is valid though, like below

    lap-sama{kouhai[1]} = #[fax: ['1', 'c'], cap: ['2', 'd']]~
    assewt(lap[fax] == [1.0, 2.0])~

    shuba-kouhai{san} = #[:]~ >_< empty hashmap
    pwint(ojou, shuba)~
    ```
    output
    ```
    #["id": 1, "age": 18] #[:]
    ```
# User Defined Types
## Groups
Groups are user defined types with fields and methods. Define a group in the
format:
```kotlin
gwoup <Group-name> {
    <field-name>-<field-type>~
}

gwoup Sample {
    property-chan~
    property2-chan~
}
```
Define a method for a group outside the group body as a function in the format:
```kotlin
fun <Group-name> <function-name>-<return-type>(<args...>) {
    <function-body...>
}

fun Sample debug-san() {
    pwint(uwu.field)~
}
```
Access the group's fields and other methods by using the `uwu` variable given.
`uwu` is a regular variable so it's immutable by default. To be able to mutate
the group's fields within the method, add a `!` after the group name:
```
fun Sample! set_field-san(val-chan) {
    uwu.field = val~
}
```

Example:
```kotlin
gwoup Sample {
    property-chan~
    property2-chan~
}
>_< methods are defined outside gwoup definition

>_< method definition. properties can be accessed using the uwu keyword
fun Sample debug-san() {
    >_< accessing fields
    pwint(uwu.property)~
}

>_< define a method that modifies state by putting ! after the gwoup name
fun Sample! modify-san(val-chan) {
    uwu.property = uwu.property + val~
    uwu.debug()~
    uwu.property2 = val~
}

fun main-san() {
    >_< all fields must be initialized
    hi test = Sample(1,2)~
    test.debug()~
    test.modify(3)~
    pwint(test.property2)~
}
```
output
```
1
4
3
```
## Contracts
Contracts define methods that `gwoups` can implement to satify it. Define a
contract in the format:
```kotlin
contwact <Contract-name> {
    <method-signature>~
}
```
Where `<method-signature>` only defines the name of the method, return type,
and the types of the arguments.
```kotlin
contwact Contract {
    method-san(kun, senpai)~
    method2-san()~
}
```
Example:
```
contwact Contract1 {
    method-san(kun, senpai)~
}

gwoup Sample {
    property-chan~
}

>_< method definition to satisfy Contract1
fun Sample method-san(a-kun, b-senpai) {
    pwint(uwu.property, a, b)~
}

fun main-san() {
    >_< this variable has a contract as a type
    hi test-Contract1 = Sample(1)~

    >_< can access methods defined in the contract
    test.method(2.0 , "three")~

    >_< but cannot access the fields of `Sample`, even if it does exist.
    pwint(test.property)~ >_< this will error
}
```
output
```
1 2.0 three
```

# String Formatting
`fowmat`
- first argument must be of type `senpai` which may or may not contain
formatting specifiers: `{}`
    - to escape braces, do: `{{` or `}}`
- following arguments are to be put in place of the formatting specifiers (`{}`)
    - following arguments' type must be a builtin owo type or implement the
    `Stringable` contract
    - missing arguments will cause a runtime error
    - extra arguments will be ignored
```kotlin
fun main-san() {
    hi name-senpai = "aqua"~
    hi age-chan = 18~
    hi blood_type-kouhai = '?'~
    
    >_< fowmat returns senpai
    sentence-senpai = fowmat(
        "My name is Minato {}! I am {} years old and my blood type is {}????",
        name,
        age,
        blood_type,
    )~
    pwint(sentence)~
}
```
output: `My name is Minato aqua! I am 18 years old and my blood type is ?????`

# Control Flow
## If-Else
Use `iwf`, `ewse`, and `ewif` for branching.
```kotlin
fun main-san() {
    hi aqua-chan = inpwt("input a number: ")~
    iwf aqua > 1 {
        pwint(aqua, "is less than 1")
    } ewif aqua == 1 {
        pwint("one")
    } ewse {
        pwint(aqua, "is more than 1")
    }
}
```
output:
```
input a number: 3
3 is more than 1
```
## Mash
Used for downcasting a variable's `contract`/`dono` type down to the
implementors. Each case only takes one type argument since union types do not
exist in owo. Cases are not fallthrough. `mash` is exhaustive, needing the
`default` case if not all implementors have a case. In the default case, the
type is not downcasted.
```kotlin
>_< this example will deconstruct the `dono` (any) type
fun main-san() {
    hi potato-dono = "string"~
    mash potato {
    chan:
        hi tato-kun = potato.to_float()~
        pwint(tato)~
    kun:
        pwint(fowmat(
                "{} was originally: {}",
                potato.to_int(),
                potato,
            )
        )~
    senpai:
        assewt(potato.len() == 6)~
        pwint("correct!")~
    default:
        >_< in this default case, dono is not downcasted and is still any
        pwint(potato)~
    }
}
```
output: `correct!`
## Loops
### For Loop
`fow init~ condition~ update { ... }`
where:
- `init` is the initial value declaration
- `condition` is the stop condition of the for loop
- `update` is the update assigned to the initial value after each iteration
```kotlin
fun main-san() {
    >_< print 1 to 3
    fow hi i-chan! = 1~ i <= 3~ i+1 {
        pwint(i)~
    }
    pwint()~
    
    >_< keep prompting the user until they type "owo"
    fow hi a-senpai! = ""~
        a == "owo"~
        inpwt("input owo: ")
    {
        fowmat("'{}'", a)~
    }
    pwint("\ndone")~
}
```
output:
```
1
2
3

'  '
input owo: no
'no'
input owo: NO!
'NO!'
input owo: owo
'owo'

done!
```
### For Each
`fow item in collection { ... }` where:
- `item` is the variable name for each item in the collection
- `collection` is the collection where its type must be a [collection type](#collection-types)
```kotlin
>_< this is a comment

fun main-san() {
    fow num in [1,2,3,4,5] {
        pwint(num)~
    }
}
```
output:
```
1
2
3
4
5
```
# Assertions
`assewt(<condition>)~` takes only one `sama` (bool) argument and will crash the
program if the condition is false
```kotlin
>_< this will crash the program
assewt(1 == 2)~
```
output: `Assertion failed: 1 == 2`
# Function Pipeline
`|` operator can be used to chain functions.
Result from the left hand side is passed as the first argument to the right hand side. Pipeline expressions must start with a variable or fn/method call, not literals or infix/prefix expressions
```kotlin
fun main-san() {
    hi aqua-chan = 2~
    aqua | pow(4) | pwint()~
    >_< equivalent to: pwint(pow(aqua, 4))~

    >_< can also just use expressions at the start
    1 + aqua * 2 | pow(4) | pwint()~
    >_< equivalent to: pwint(pow(1 + aqua * 2, 4))~

    [6,5,4,3,2,1]
    | filter_evens()
    | sort_ascending()
    | pwint()~
    >_< equivalent to: pwint(sort_ascending(filter_evens(totally_random_vec_of_nums())))
}

fun pow-chan(num-chan, exp-chan) {
    hi res! = num~
    fow hi i = 1~ i < exp~ i + 1 {
        res *= exp~
    }
    wetuwn res~
}

fun filter_evens-chan[1](vec-chan[1]) {
    hi res-chan[1]! = []~
    fow num in vec {
        iwf num % 2 == 0 {
            res.push(num)~
        }
    }
    wetuwn res~
}

fun sort_ascending-chan[1](vec-chan[1]) {
    hi res-chan[1]! = vec~
    fow hi i-chan = 1~ i <= res.len()~ i+1 {
        fow hi j-chan = i+1~ j <= res.len()~ j+1 {
            hi outer-chan = res.nth(i).unwrap_or(0)~
            hi inner-chan = res.nth(j).unwrap_or(0)~
            iwf outer < inner { res.swap(i, j)~ }
        }
    }
    wetuwn res~
}
```
