# OwO
compiler for [UwU++](https://github.com/Gidsss/UwUIDE) written in rust.
Language Design is overhauled to have support for optional types, immutable variables by default, sets, maps, assertions, and more!

# Contents
- [Hello World](#hello-world)
- [IO](#io)
- [Declaration and Assignment](#declaration-and-assignment)
- [Function Declaration and Calling](#function-declaration-and-calling)<br><br>
- [Type System](#type-system)
    - [Primitive Types](#primitive-types)
    - [Optional Type](#optional-type)
    - [Collection Types](#collection-types)<br><br>
- [User Defined Types](#user-defined-types)
    - [Groups](#groups)
    - [Contracts](#contracts)<br><br>
- [Control Flow](#control-flow)
    - [Mash](#mash)
    - [If-Else](#if-else)
    - [Loops](#loops)
        - [For Loop](#for-loop)
        - [For Each](#for-each)<br><br>
- [Assertions](#assertions)
- [Function Pipeline](#function-pipeline)
---

# Hello World
All programs must have a main function
- must have no arguments
- must return nothing (aka return type `san`)
```kotlin
>.< this is a comment

fun main-san() {
    pwint("hello", "world")~
}
```
output: `hello world`
# IO
Printing to console can be done using the builtin `pwint` function.
- takes a variable amount of arguments of any type.
- each argument is separated by space when printed.

Taking input from user can be done using the builtin `inpwt` function.
- takes a variable amount of arguments of any type
- args will be printed to the console as the prompt
- will read user input until the user presses enter.
- user input will be implicitly converted to the type declared if `inpwt()` is
the right hand side of a declaration/assignment
- `inpwt()` can also be a standalone statement, not needing to be on the right hand side of a declaration/assignment

```kotlin
>.< this is a comment

fun main-san() {
    >.< name is mutable, inpwt is implicitly converted to senpai
    name-senpai! = inpwt("what is your first name?: ")~
    pwint("hello", name, "\n")~

    >.< inpwt is implicitly converted to senpai even in assignments
    name = inpwt("what is your last name?: ")~
    pwint(name, "is a nice surname\n")~

    inpwt("Press Enter to exit...")~
}
```
output:
```
what is your name?: owo
hello owo

what is your last name?: uwu
uwu is a nice surname

Press Enter to exit...
```
# Declaration and Assignment
1. Variable declarations are in the format:<br>
`hi name-type = value~`<br>

2. Variables are immutable by default. To declare mutable variables, put a `!` after the type:<br>
`hi name-type! = value~`

3. Assignments are done in the format:<br>
`name = value~`.

```kotlin
>.< this is mutable
hi global-chan! = 99~ 

fun main-san() {
    >.< this is constant
    hi num-chan = 1~ 

    >.< reassigning global variable
    global = 0~
    pwint(global, num, global_2)~
}

hi global_2-chan = 2~
```
output: `0 1 2`
# Function Declaration and Calling
Function declarations are in the format:<br>
`fun name-type(param-type, param2-type, ...params) { ... }`<br>
where:
```kotlin
>.< this is a comment

fun main-san() {
    pwint(sum(2,3))~
}

>.< args: one immutable and one mutable int
>.< returns: int
fun sum-chan(left-chan, right-chan!) {
    wetuwn left + right~
}
```
output: `5`

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
<br>

4. `kouhai`: char<br>
    declaration: `hi pe-kouhai = 'o'~`<br>
    `kouhai` literals are enclosed in `'` and can only contain one character<br>
<br>

5. `sama`: boolean<br>
    declaration: `hi lap-sama = fax~` (True)<br>
    declaration: `hi lap-sama = cap~` (False)<br>
<br>

5. `san`: null
    - used for functions that never return anything
    - identifiers that have this type can only have the value `nuww` (null)<br>
    function declaration: `fun main-san() { ... }`<br>
    declaration: `hi aqua-san = nuww~`<br>
<br>

6. `dono`: any<br>
    `hi aqua-dono = nuww~`<br>
    `hi shion-dono = 1~`<br>
    `hi ojou-dono = "hello"~`<br>
    - `dono` can be downcasted to any other type using a [`mash`](#mash) statement

## Optional Type
- to declare a variable with an optional type, use the `?` symbol in the format: `hi name-type?`
- the value of an option type can either be a value satisfying the type declared or `nuww`
- note that you would still have to `.unwrap()` or `.unwrap_or()` to use the value
```kotlin
>.< optional chan can have an int or nuww as a value
hi aqua-chan? = 1~
hi shion-chan? = nuww~

>.< adding ! before or after ? turns it into an mutable optional variable
hi ojou-senpai!? = "hello"~
ojou = nuww~

assewt(aqua.unwrap_or(0) + shion.unwrap_or(0) == 1)~
assewt(ojou.unwrap_or(0) == 0)~
```

## Collection Types
### Collections
1. **vector**
    - defined as `type[dim]` where `type` is any type and `dim` is an integer literal<br>
    - note that `dim` cannot be an identifier<br>
    - vector literals must be enclosed in square brackets: `[item]`
    - to get an item from a vector, use:
        1. `.nth()` method: `aqua.nth(1)`
            - `aqua.nth(1)` gets the first item
            - this returns an optional type where you have to `.unwrap()`
            to use the value
        2. subscript operator `[]`: `aqua[1]`
            - `aqua[1]` gets the first item
            - `aqua[1]` is equivalent to `aqua.nth(1).unwrap()`
    ```kotlin
    aqua-chan[1] = [1, 2, 3]~
    shion-chan[2] = [[1,2], [3,4], [5,6]]~

    assewt(aqua.nth(1).unwrap() == 1)~
    assewt(shion[1][2] == 2)~

    pwint(aqua)~
    ```
    output
    ```
    [1, 2, 3]
    ```
<br>

2. **set**
    - defined as `type{}` where `type` is any type<br>
    - set literals must be enclosed in curly brackets `{item}`
    - a set ensures only unique items are in the set even if user put 
    in duplicates
    - you cannot get items from a set
    ```kotlin
    aqua-senpai{} = { "1", "1", "2", "3" }~
    pwint(aqua)~
    ```
    output
    ```
    {"1", "2", "3"}
    ```
<br>

3. **map**
    - defined as `key{val}` where both `key` and `val` are any type<br>
    - map literals must be enclosed in curly brackets with key and value separated by colon `{key:val}`
    - to get an item from a map, use:
        1. `.get()` method: `aqua.get("item")`
            - this returns an optional type where you have to `.unwrap()`
            to use the value
        2. subscript operator `[]`: `aqua["item"]`
            - `aqua["item"]` is equivalent to `aqua.get("item").unwrap()`
    ```kotlin
    ojou-senpai{chan} = { "id":1, "age":18 }~
    lap-sama{kun[1]} = { fax:[1.0, 2.0], cap:[3.0, 4.0] }~

    assewt(ojou.get("age").unwrap() == 18)~
    assewt(lap[fax] == [1.0, 2.0])~

    pwint(ojou)~
    ```
    output
    ```
    {"id": 1, "age": 18}~
    ```
# User Defined Types
## Groups
1. Groups are user defined types with fields and methods<br>
2. Format:
    - start with `gwoup` keyword followed by name starting with capital letter
    - optional [contracts](#contracts) implemented enclosed in `[]`
    - body enclosed in `{}`<br>
<br>

3. Inside body, user can declare fields and methods
    - fields format: `name-type~`<br>
    - methods format: `fun name-type(uwu, ...params) { ... }`<br>
<br>

4. Methods need mandatory `uwu` or `uwu!` as first argument. These lets you access fields and other methods
    - `uwu!` means the method might mutate the state of the group<br>
    - `uwu` as a parameter means the method does not mutate the state of the group
        - within the method, user can only use other methods that also take in `uwu`
        - user cannot assign to properties or use methods that take in `uwu!`<br>
<br>

5. Declare a variable with a `cwass` type in this format:<br>
`hi name-class = class(...fields)~`
    - all fields must be initialized
```kotlin
>.< this group does not implement any contracts
gwoup Sample {
    property-chan~

    >.< method that does not mutate state
    fun method-san(uwu) {
        >.< accessing fields
        pwint(uwu.property)~
    }

    >.< method that mutates state
    fun method2-san(uwu!, val-chan) {
        uwu.property = uwu.property + uwu.property2~
        uwu.property2 = val~
    }

    >.< there is no strict order in whether fields or methods should be defined first
    property2-chan~
}

fun main-san() {
    hi test-Sample = Sample(1,2)~
    test.method()~
    test.method2(3)~
}
```
## Contracts
1. Contracts define methods that `gwoups` must implement if put in a group's definition
2. Format:<br>
    - start with `contwact` keyword
    - then identifier that starts with a capital letter
    - then body enclosed in `{}`<br>
<br>

3. Inside body, user can declare methods
    - format: `name-type(self, ...params)~`
    - methods here have no body, just the signature<br>
<br>

4. variables can have a `contract` type
```
contwact Contract1 {
    method-san(self)~
}
gwoup Sample [Contract1] {
    property-chan~

    fun method-san(uwu) {
        pwint(uwu.property)~
    }
}
fun main-san() {
    >.< this variable has a contract as a type
    hi test-Contract1 = Sample(1)~

    >.< can access methods defined in the contract
    test.method()~

    >.< but cannot access the fields of `Sample`
    pwint(test.property)~ >.< this will error
}

>.< this group does not fully implement the contract Contract1
>.< this will cause a compile time error
gwoup Sample2 [Contract1] {
    property-chan~
}
```

# Control Flow
## Mash
Used for deconstructing a variable's `contract`/`dono` type down to the implementors
- `mash` is exhaustive, needing the `default` case if not all implementors have a case
- in the default case, the type is not downcasted
```kotlin
>.< this example will deconstruct the `dono` (any) type
fun main-san() {
    hi potato-dono = "string"~
    mash potato {
        chan {
            pwint(potato + 1)~
        }
        kun {
            pwint(potato / 2.2)~
        }
        senpai {
            assewt(potato.len() == 6)~
        }
        default {
            >.< in this default case, dono is not downcasted and is still any
            pwint(potato)~
        }
    }
}
```
## If-Else
Use `iwf`, `ewse`, and `ewif` for branching.
```kotlin
fun main-san() {
    aqua-chan = inpwt("input a number: ")~
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
## Loops
### For Loop
Format is:<br>
`fow init~condition~update { ... }`<br>
where:
- `init` is the initial value declaration
- `condition` is the stop condition
- `update` is the update assigned to the initial value
```kotlin
>.< this is a comment

fun main-san() {
    >.< print 1 to 3
    fow hi i-chan! = 1~ i <= 3~ i+1 {
        pwint(i)~
    }
    pwint()~
    
    >.< keep prompting the user until they type "owo"
    fow hi a-senpai! = ""~
        a == "owo"~
        inpwt("input owo: ")
    {
        pwint("'", a, "'")~
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
' no '
input owo: NO!
' NO! '
input owo: owo
' owo '

done!
```
### For Each
Format is:<br>
`fow item in collection { ... }`<br>
where:
- `item` is the variable name for each item in the collection
- `collection` is the collection
```kotlin
>.< this is a comment

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
Assertions are in the format: `assewt(<condition>)~`
- builtin `assewt` function takes only one `sama` (bool) argument
- will crash the program if the condition is false
```kotlin
>.< this will crash the program
assewt(1 == 2)~
```
output: `Assertion failed: 1 == 2`
# Function Pipeline
`|` operator can be used to chain functions.
Result from the left hand side is passed as the first argument to the right hand side
```kotlin
fun main-san() {
    hi aqua-chan = 2~
    aqua | pow(4) | pwint()~
    >.< equivalent to: pwint(pow(aqua, 4))~

    totally_random_vec_of_nums()
    | filter_evens()
    | sort_ascending()
    | pwint()~
    >.< equivalent to: pwint(sort_ascending(filter_evens(totally_random_vec_of_nums())))
}

fun pow-chan(num-chan, exp-chan) {
    wetuwn num ^ exp~
}
fun totally_random_vec_of_nums-chan[1]() {
    wetuwn [6,5,4,3,2,1]~
}
fun filter_evens-chan[1](vec-chan[1]) {
    hi res-chan[1]! = []~
    fow num in vec {
        iwf num % 2 == 0 {
            res.push(num)~
        }
    }
}
fun sort_ascending-chan[1](vec-chan[1]) {
    hi res-chan[1]! = vec~
    fow hi i-chan = 1~ i < res.len()~ i+1 {
        fow hi j-chan = i+1~ j < res.len()~ j+1 {
            hi outer-chan = res.nth(i).unwrap_or(0)~
            hi inner-chan = res.nth(j).unwrap_or(0)~
            iwf outer < inner { res.swap(i, j)~ }
        }
    }
    wetuwn res~
}
