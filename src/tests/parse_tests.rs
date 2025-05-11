use crate::{
    lexer::{lexer::Lexer, token::TokenKind},
    parser::{parser::Parser, productions::Production},
    utils::string::StringExt,
};

/// asserts that passed in formatted source is equal to how the parser formats it
fn assert_already_formatted(formatted: &'static str) -> () {
    assert_formatted_eq(formatted.to_string(), formatted)
}
/// asserts that source, which will be formatted by parser, is equal to the passed in formatted
/// string
fn assert_formatted_eq(source: String, formatted: &'static str) -> () {
    let lexer = Lexer::new(source);
    let mut parser = Parser::new(
        &lexer.source,
        &lexer.line_starts,
        lexer
            .tokens
            .into_iter()
            .filter(|tok| match tok.kind {
                TokenKind::Whitespace
                | TokenKind::Newline
                | TokenKind::Tab
                | TokenKind::CarriageReturn
                | TokenKind::Comment
                | TokenKind::EOF => false,
                _ => true,
            })
            .collect(),
    );
    let _ = parser.parse_program();
    assert_eq!(
        formatted,
        parser.program.to_formatted_string(&lexer.source, 0),
        "expected:\n{}\n===\nactual:\n{}",
        formatted,
        parser.program.to_formatted_string(&lexer.source, 0),
    )
}

// /// asserts that the passed in source string is an error with the same header
// /// as the given header.
// fn assert_error(error_header: &str, source: &str) {
//     let lexer = Lexer::new(source.to_string());
//     let mut parser = Parser::new(
//         &lexer.source,
//         &lexer.line_starts,
//         lexer
//             .tokens
//             .into_iter()
//             .filter(|tok| match tok.kind {
//                 TokenKind::Whitespace
//                 | TokenKind::Newline
//                 | TokenKind::Tab
//                 | TokenKind::CarriageReturn
//                 | TokenKind::Comment
//                 | TokenKind::EOF => false,
//                 _ => true,
//             })
//             .collect(),
//     );
//     assert!(
//         parser
//             .errors
//             .iter()
//             .map(|err| err.to_string())
//             .nth(0)
//             .unwrap()
//             .starts_with(&error_header.red()),
//         "Expected {}\nGot {}",
//         error_header,
//         parser
//             .errors
//             .iter()
//             .map(|err| err.to_string())
//             .nth(0)
//             .unwrap()
//     );
// }

#[test]
fn hello_world() {
    let source = r#"fun main-san(){pwint("hello world")~}"#;
    let formatted = r#"
fun main-san() {
    pwint("hello world")~
}"#
    .trim();
    assert_formatted_eq(source.to_string(), formatted);
    assert_already_formatted(formatted);
}

#[test]
fn hello_world_from_method() {
    let source = r#"fun Hololive hello-san()
        {pwint("hello world")~}
        fun main-san()
        {pwint("hello world")~}"#;
    let formatted = r#"
fun main-san() {
    pwint("hello world")~
}
fun Hololive hello-san() {
    pwint("hello world")~
}"#
    .trim();
    assert_formatted_eq(source.to_string(), formatted);
    assert_already_formatted(formatted);
}

#[test]
fn declaration_and_assignment() {
    let source = r#"
    fun main-san() {
        hi aqua-chan=1~hi aqua-kun=2.0~
        hi aqua=1~hi aqua=2.0~
        hi aqua-sama=fax~hi aqua-senpai="string"~
        hi aqua=fax~hi aqua="string"~
        hi aqua-kouhai='c'~hi aqua-san=nuww~
        hi aqua='c'~hi aqua=nuww~
        hi aqua-dono=any(1,2,3,)~hi aqua-Class=Class(4,5,6,)~
        hi aqua=any(1,2,3,)~hi aqua=Class(4,5,6,)~
        hi aqua-chan=6~hi aqua=6~aqua=7~
        aqua.hands=8~aqua.scream().pitch=9~
    }"#;
    let formatted = r#"
fun main-san() {
    hi aqua-chan = 1~
    hi aqua-kun = 2.0~
    hi aqua = 1~
    hi aqua = 2.0~
    hi aqua-sama = fax~
    hi aqua-senpai = "string"~
    hi aqua = fax~
    hi aqua = "string"~
    hi aqua-kouhai = 'c'~
    hi aqua-san = nuww~
    hi aqua = 'c'~
    hi aqua = nuww~
    hi aqua-dono = any(1, 2, 3)~
    hi aqua-Class = Class(4, 5, 6)~
    hi aqua = any(1, 2, 3)~
    hi aqua = Class(4, 5, 6)~
    hi aqua-chan = 6~
    hi aqua = 6~
    aqua = 7~
    aqua.hands = 8~
    aqua.scream().pitch = 9~
}"#
    .trim();
    assert_formatted_eq(source.to_string(), formatted);
    assert_already_formatted(formatted);
}

#[test]
fn global_declarations() {
    let source = r#"
hi aqua-chan=1~hi aqua-kun=2.0~
hi aqua=1~hi aqua=2.0~
hi aqua-sama=fax~hi aqua-senpai="string"~
hi aqua=fax~hi aqua="string"~
hi aqua-kouhai='c'~hi aqua-san=nuww~
hi aqua='c'~hi aqua=nuww~
hi aqua-dono=any(1,2,3,)~hi aqua-Class=Class(4,5,6,)~
hi aqua=any(1,2,3,)~hi aqua=Class(4,5,6,)~
hi aqua-chan=6~hi aqua=6~fun main
-san(){pwint()
~}"#;
    let formatted = r#"
hi aqua-chan = 1~
hi aqua-kun = 2.0~
hi aqua = 1~
hi aqua = 2.0~
hi aqua-sama = fax~
hi aqua-senpai = "string"~
hi aqua = fax~
hi aqua = "string"~
hi aqua-kouhai = 'c'~
hi aqua-san = nuww~
hi aqua = 'c'~
hi aqua = nuww~
hi aqua-dono = any(1, 2, 3)~
hi aqua-Class = Class(4, 5, 6)~
hi aqua = any(1, 2, 3)~
hi aqua = Class(4, 5, 6)~
hi aqua-chan = 6~
hi aqua = 6~
fun main-san() {
    pwint()~
}
"#
    .trim();
    assert_formatted_eq(source.to_string().to_string(), formatted);
    assert_already_formatted(formatted);
}
#[test]
fn optional_types() {
    let source = r#"
fun main-san() {
    hi aqua-chan?=nuww~hi aqua-chan?! =1~
    hi aqua-kun?[1]?=nuww~hi aqua-kun[1]?! =[2.0,]~
    hi aqua-sama?{}?=nuww~hi aqua-sama{}?! =#[fax,]~
    hi aqua-senpai?{kouhai?}?=nuww~hi aqua-senpai{kouhai}?! =#["string": 's',]~
    hi aqua-dono?{san?}?=nuww~hi aqua-dono{san}?! =#[any(1,2,3,): nuww,]~
    hi aqua-Class?=nuww~hi aqua-Class?! =Class(4,5,6,)~
}
fun Sample! mutable_method-san(other-chan,variadic-chan...?){uwu.property+=other~}
fun Sample! another_mutable_method-san(another_variadic-chan?...?){uwu.property+=other~}"#;
    let formatted = r#"
fun main-san() {
    hi aqua-chan? = nuww~
    hi aqua-chan?! = 1~
    hi aqua-kun?[1]? = nuww~
    hi aqua-kun[1]?! = [2.0]~
    hi aqua-sama?{}? = nuww~
    hi aqua-sama{}?! = #[fax]~
    hi aqua-senpai?{kouhai?}? = nuww~
    hi aqua-senpai{kouhai}?! = #["string": 's']~
    hi aqua-dono?{san?}? = nuww~
    hi aqua-dono{san}?! = #[any(1, 2, 3): nuww]~
    hi aqua-Class? = nuww~
    hi aqua-Class?! = Class(4, 5, 6)~
}
fun Sample! mutable_method-san(other-chan, variadic-chan...?) {
    uwu.property += other~
}
fun Sample! another_mutable_method-san(another_variadic-chan?...?) {
    uwu.property += other~
}"#
    .trim();
    assert_formatted_eq(source.to_string(), formatted);
    assert_already_formatted(formatted);
}

#[test]
fn vector_types() {
    let source = r#"
fun main-san() {
    hi aqua-chan[1]=[1,2,3]~hi aqua-kun[1]=[1.0,2.0,3.0]~
    hi aqua-sama[1]=[fax,cap,fax]~hi aqua-senpai[1]=["11","22","33"]~
    hi aqua-kouhai[1]=['1','2','3']~hi aqua-san[1]=[nuww,nuww,nuww]~
    hi aqua-Class[1]=[Class(1,2,3),Class(4,5,6),Class(7,8,9)]~hi aqua-dono[2]=[[1,2,3],[1.0,2.0,3.0],[fax,cap,fax]]~
    aqua=[["11","22","33"],['1','2','3'],[nuww,nuww,nuww]]~aqua.hands=[Class(1,2,3),Class(4,5,6),Class(7,8,9)]~
    aqua.scream().pitch=[[1,2,3],[1.0,2.0,3.0],[fax,cap,fax]]~
}"#;
    let formatted = r#"
fun main-san() {
    hi aqua-chan[1] = [1, 2, 3]~
    hi aqua-kun[1] = [1.0, 2.0, 3.0]~
    hi aqua-sama[1] = [fax, cap, fax]~
    hi aqua-senpai[1] = ["11", "22", "33"]~
    hi aqua-kouhai[1] = ['1', '2', '3']~
    hi aqua-san[1] = [nuww, nuww, nuww]~
    hi aqua-Class[1] = [Class(1, 2, 3), Class(4, 5, 6), Class(7, 8, 9)]~
    hi aqua-dono[2] = [[1, 2, 3], [1.0, 2.0, 3.0], [fax, cap, fax]]~
    aqua = [["11", "22", "33"], ['1', '2', '3'], [nuww, nuww, nuww]]~
    aqua.hands = [Class(1, 2, 3), Class(4, 5, 6), Class(7, 8, 9)]~
    aqua.scream().pitch = [[1, 2, 3], [1.0, 2.0, 3.0], [fax, cap, fax]]~
}"#
    .trim();
    assert_formatted_eq(source.to_string(), formatted);
    assert_already_formatted(formatted);
}

#[test]
fn hashset_types() {
    let source = r#"
fun main-san() {
    hi aqua-chan{}=#[1,2,3]~ hi aqua-kun{}=#[1.0,2.0,3.0]~
    hi aqua-sama{}=#[fax,cap,fax]~ hi aqua-senpai{}=#["11","22","33"]~
    hi aqua-kouhai{}=#['1','2','3']~ hi aqua-san{}=#[nuww,nuww,nuww]~
    hi aqua-Class{}=#[Class(1,2,3),Class(4,5,6),Class(7,8,9)]~hi aqua-dono{chan{}[1]}[1] = [#[1,2,3],#[1.0,2.0,3.0,],#[fax,cap,fax],]~
    aqua=[#["11","22","33"],#['1','2','3'],#[nuww,nuww,nuww]]~ aqua.hands=#[Class(1,2,3),Class(4,5,6),Class(7,8,9)]~
    aqua.scream().pitch = [#[1,2,3],#[1.0,2.0,3.0],#[fax,cap,fax]]~
}"#;
    let formatted = r#"
fun main-san() {
    hi aqua-chan{} = #[1, 2, 3]~
    hi aqua-kun{} = #[1.0, 2.0, 3.0]~
    hi aqua-sama{} = #[fax, cap, fax]~
    hi aqua-senpai{} = #["11", "22", "33"]~
    hi aqua-kouhai{} = #['1', '2', '3']~
    hi aqua-san{} = #[nuww, nuww, nuww]~
    hi aqua-Class{} = #[Class(1, 2, 3), Class(4, 5, 6), Class(7, 8, 9)]~
    hi aqua-dono{chan{}[1]}[1] = [#[1, 2, 3], #[1.0, 2.0, 3.0], #[fax, cap, fax]]~
    aqua = [#["11", "22", "33"], #['1', '2', '3'], #[nuww, nuww, nuww]]~
    aqua.hands = #[Class(1, 2, 3), Class(4, 5, 6), Class(7, 8, 9)]~
    aqua.scream().pitch = [#[1, 2, 3], #[1.0, 2.0, 3.0], #[fax, cap, fax]]~
}"#
    .trim();
    assert_formatted_eq(source.to_string(), formatted);
    assert_already_formatted(formatted);
}

#[test]
fn hashmap_types() {
    let source = r#"
fun main-san() {
    hi aqua-chan{kun}=#[1: 2.0,3: 4.0,5: 6.0]~hi aqua-sama{senpai}=#[fax: "11",cap: "22"]~
    hi aqua-kouhai{san}=#['1': nuww,'2': nuww,'3': nuww]~hi aqua-dono{Class}=#[7: Class(1,2,3),8.0: Class(4,5,6),"99": Class(7,8,9)]~
    hi aqua-dono{chan{}[1]}[1]=[#[any: [#[1,2,3],#[4,5,6]]]]~aqua=#['1': nuww,'2': nuww,'3': nuww]~
    aqua.hands=#[7: Class(1,2,3),8.0: Class(4,5,6),"99": Class(7,8,9)]~aqua.scream().pitch=[#[any: [#[1,2,3],#[4,5,6]]]]~
}"#;
    let formatted = r#"
fun main-san() {
    hi aqua-chan{kun} = #[1: 2.0, 3: 4.0, 5: 6.0]~
    hi aqua-sama{senpai} = #[fax: "11", cap: "22"]~
    hi aqua-kouhai{san} = #['1': nuww, '2': nuww, '3': nuww]~
    hi aqua-dono{Class} = #[7: Class(1, 2, 3), 8.0: Class(4, 5, 6), "99": Class(7, 8, 9)]~
    hi aqua-dono{chan{}[1]}[1] = [#[any: [#[1, 2, 3], #[4, 5, 6]]]]~
    aqua = #['1': nuww, '2': nuww, '3': nuww]~
    aqua.hands = #[7: Class(1, 2, 3), 8.0: Class(4, 5, 6), "99": Class(7, 8, 9)]~
    aqua.scream().pitch = [#[any: [#[1, 2, 3], #[4, 5, 6]]]]~
}"#
    .trim();
    assert_formatted_eq(source.to_string(), formatted);
    assert_already_formatted(formatted);
}

#[test]
fn group_and_contract_definition() {
    let source = r#"fun main-san(){pwint()~}
    gwoup HololiveMember{name-senpai~age-kun~manager-senpai~}
    contwact Idol{sing-senpai(chan, kun,)~dance-senpai(kouhai, sama,)~}"#;
    let formatted = r#"
fun main-san() {
    pwint()~
}
gwoup HololiveMember {
    name-senpai~
    age-kun~
    manager-senpai~
}
contwact Idol {
    sing-senpai(chan, kun)~
    dance-senpai(kouhai, sama)~
}"#
    .trim();
    assert_formatted_eq(source.to_string(), formatted);
    assert_already_formatted(formatted);
}

#[test]
fn binary_operations_with_precedence() {
    let source = r#"
fun main-san() {
    >_< math ops
    hi aqua-chan = 1+2-3~
    aqua = 5-6*7~
    aqua = 8*(9+1)/2~
    aqua = -3*4+5*-6~

    >_< rel ops
    hi ojou-dono = 1>2<3~
    ojou = 4>=5<=6~
    ojou = 7==8!=9~

    >_< eq ops
    hi shion-sama = 1 or 2 or 3~
    shion = 4 or 5 and 6~
    shion = 7 and 8 or 9~
    shion = 1 and 2 and 3~
    shion = 1 and 2 or 3 and 4~
    shion = not 5 and 6 or 7~

    >_< all combined ops
    hi lap-sama = -1+-2>=-3*4==5 or 6 and not 7~
}"#
    .trim();
    let formatted = r#"
fun main-san() {
    hi aqua-chan = 1 + 2 - 3~
    aqua = 5 - 6 * 7~
    aqua = 8 * (9 + 1) / 2~
    aqua = -3 * 4 + 5 * -6~
    hi ojou-dono = 1 > 2 < 3~
    ojou = 4 >= 5 <= 6~
    ojou = 7 == 8 != 9~
    hi shion-sama = 1 or 2 or 3~
    shion = 4 or 5 and 6~
    shion = 7 and 8 or 9~
    shion = 1 and 2 and 3~
    shion = 1 and 2 or 3 and 4~
    shion = not 5 and 6 or 7~
    hi lap-sama = -1 + -2 >= -3 * 4 == 5 or 6 and not 7~
}"#
    .trim();
    assert_formatted_eq(source.to_string(), formatted)
}

#[test]
fn function_and_method_parameters() {
    let source = r#"
    fun main-san(){pwint()~}
    fun short_function-san(param-chan,param-kun,){pwint()~}
    fun long_function-san(param-chan,param-kun,param-sama,param-san,param-senpai,param-kouhai,param-dono,param-Class,)
    { pwint()~
    }
    fun Class short_function-san(param-chan,param-kun,){pwint()~}
    fun Class!long_method-san(param-chan,param-kun,param-sama,param-san,param-senpai,param-kouhai,param-dono,param-Class,)
    { pwint()~
    }"#;
    let formatted = r#"
fun main-san() {
    pwint()~
}
fun short_function-san(param-chan, param-kun) {
    pwint()~
}
fun long_function-san(
    param-chan,
    param-kun,
    param-sama,
    param-san,
    param-senpai,
    param-kouhai,
    param-dono,
    param-Class,
) {
    pwint()~
}
fun Class short_function-san(param-chan, param-kun) {
    pwint()~
}
fun Class! long_method-san(
    param-chan,
    param-kun,
    param-sama,
    param-san,
    param-senpai,
    param-kouhai,
    param-dono,
    param-Class,
) {
    pwint()~
}"#
    .trim();
    assert_formatted_eq(source.to_string(), formatted);
    assert_already_formatted(formatted);
}

#[test]
fn if_elif_else() {
    let source = r#"
fun main-san() {
iwf condition { pwint()~ }
ewif condition2 {
iwf inner_condition { pwint()~ }
ewif inner_condition2 { pwint()~ }
ewse { pwint()~ } }
ewif condition3 { pwint()~ }
ewse { pwint()~ }
}"#;
    let formatted = r#"
fun main-san() {
    iwf condition {
        pwint()~
    } ewif condition2 {
        iwf inner_condition {
            pwint()~
        } ewif inner_condition2 {
            pwint()~
        } ewse {
            pwint()~
        }
    } ewif condition3 {
        pwint()~
    } ewse {
        pwint()~
    }
}"#
    .trim();
    assert_formatted_eq(source.to_string(), formatted);
    assert_already_formatted(formatted);
}

#[test]
fn mash_statement() {
    let source = r#"
fun main-san() {
    mash potato {
    chan:pwint()~kun[1]:pwint()~pwint()~
    sama{}:pwint()~senpai{kouhai}:pwint()~pwint()~
    dono{san}[2]:pwint()~Class:pwint()~pwint()~
    default:pwint()~
    }
}"#;
    let formatted = r#"
fun main-san() {
    mash potato {
    chan: pwint()~
    kun[1]:
        pwint()~
        pwint()~
    sama{}: pwint()~
    senpai{kouhai}:
        pwint()~
        pwint()~
    dono{san}[2]: pwint()~
    Class:
        pwint()~
        pwint()~
    default: pwint()~
    }
}"#
    .trim();
    assert_formatted_eq(source.to_string(), formatted);
    assert_already_formatted(formatted);
}

#[test]
fn for_loops() {
    let source = r#"
fun main-san() {
    fow
    hi i-chan! =1~
    i <= 3~
    i+1
    { pwint()~ }
    fow hi a-senpai! =""~a=="owo"~inpwt("input owo: "){pwint()~}
}"#;
    let formatted = r#"
fun main-san() {
    fow hi i-chan! = 1~ i <= 3~ i + 1 {
        pwint()~
    }
    fow hi a-senpai! = ""~ a == "owo"~ inpwt("input owo: ") {
        pwint()~
    }
}"#
    .trim();
    assert_formatted_eq(source.to_string(), formatted);
    assert_already_formatted(formatted);
}

#[test]
fn for_each_loops() {
    let source = r#"
fun main-san(){fow num in [1,2,3,4,5,]{pwint(num)~}}"#;
    let formatted = r#"
fun main-san() {
    fow num in [1, 2, 3, 4, 5] {
        pwint(num)~
    }
}"#
    .trim();
    assert_formatted_eq(source.to_string(), formatted);
    assert_already_formatted(formatted);
}

#[test]
fn function_pipelines() {
    let source = r#"
fun main - san() { "hewwo world~".chars().unique()[ 3].to_int()-10*7/3 |pow(4) 
|pwint()~[1,2,3,4,5, 6, 7,8, 9] |filter_evens()|sort_ascending()|
    pwint("placeholder so the pipeline becomes long enough")~
[1, 2, 3, 4, 5][1].abs() + 9 * 8 / 6 - 5 | (1 + 2 * 3).pow() | pwint(4)~
hi aqua-chan = "konaqua~".upper() | shuffle_string() | md5hash()~
shion = "eeeehhh?"[1].to_int()|average([5, 2, 5, 7, 2, 2].unique().sum(),
"random long string".chars() | charlist_to_int() | list_sum(),)
|Namespace().method('o',[90.3,62.5],arg3)|(4/5-6*7 + 8).max(10,20)~
ojou = (Namespace(args, extra_arg).field|Namespace().add()).field~}"#;
    let formatted = r#"
fun main-san() {
    "hewwo world~".chars().unique()[3].to_int() - 10 * 7 / 3 | pow(4) | pwint()~
    [1, 2, 3, 4, 5, 6, 7, 8, 9]
    | filter_evens()
    | sort_ascending()
    | pwint("placeholder so the pipeline becomes long enough")~
    [1, 2, 3, 4, 5][1].abs() + 9 * 8 / 6 - 5 | (1 + 2 * 3).pow() | pwint(4)~
    hi aqua-chan = "konaqua~".upper() | shuffle_string() | md5hash()~
    shion = "eeeehhh?"[1].to_int()
        | average(
            [5, 2, 5, 7, 2, 2].unique().sum(),
            "random long string".chars() | charlist_to_int() | list_sum(),
        )
        | Namespace().method('o', [90.3, 62.5], arg3)
        | (4 / 5 - 6 * 7 + 8).max(10, 20)~
    ojou = (Namespace(args, extra_arg).field | Namespace().add()).field~
}"#
    .trim();
    assert_formatted_eq(source.to_string().to_string(), formatted);
    assert_already_formatted(formatted);
}

#[test]
fn literals_accessing_methods_and_fields() {
    let source = r#"
fun main - san (){1.abs()|2.max()~1.5.round()|2.5.round().max()~fax.or_(cap)|
cap.and_()~cap . and_()| fax.or_(cap)~"string". chars() |
"this is a ".extend()~'1'.to_int()|'2'.to_string
().nth().unwrap()~Namespace(args,extra_arg).field|Namespace().add()~}"#;
    let formatted = r#"
fun main-san() {
    1.abs() | 2.max()~
    1.5.round() | 2.5.round().max()~
    fax.or_(cap) | cap.and_()~
    cap.and_() | fax.or_(cap)~
    "string".chars() | "this is a ".extend()~
    '1'.to_int() | '2'.to_string().nth().unwrap()~
    Namespace(args, extra_arg).field | Namespace().add()~
}"#
    .trim();
    assert_formatted_eq(source.to_string().to_string(), formatted);
    assert_already_formatted(formatted);
}

// #[test]
// fn error_no_main() {
//     assert_error(
//         &"[NO MAIN FUNCTION]".red(),
//         r#"
// fun func-san() {
//     pwint("hewwo world~")~
// }"#,
//     );
// }
//
// #[test]
// fn error_empty_function_body() {
//     assert_error(
//         &"[EMPTY FUNCTION BODY]".red(),
//         r#"
// fun main-san() {}
// "#,
//     );
// }
//
// #[test]
// fn error_empty_if_body() {
//     assert_error(
//         &"[EMPTY IF BODY]".red(),
//         r#"
// fun main-san() {
//     iwf fax {}
// }
// "#,
//     );
// }
//
// #[test]
// fn error_empty_for_body() {
//     assert_error(
//         &"[EMPTY FOR BODY]".red(),
//         r#"
// fun main-san() {
//     fow i in [1,2,3,4,5] {}
// }
// "#,
//     );
// }
//
// #[test]
// fn error_empty_group_body() {
//     assert_error(
//         &"[EMPTY GROUP BODY]".red(),
//         r#"
// fun main-san() { pwint()~ }
// gwoup Sample {}
// "#,
//     );
// }
//
// #[test]
// fn error_empty_mash_case_body() {
//     assert_error(
//         &"[EMPTY MASH BODY]".red(),
//         r#"
// fun main-san() {
//     mash val {}
// }
// "#,
//     );
// }
//
// #[test]
// fn error_empty_mash_body() {
//     assert_error(
//         &"[EMPTY MASH CASE BODY]".red(),
//         r#"
// fun main-san() {
//     mash val {
//     chan:
//     }
// }
// "#,
//     );
// }
//
// #[test]
// fn error_empty_method_body() {
//     assert_error(
//         &"[EMPTY METHOD BODY]".red(),
//         r#"
// fun main-san() { pwint()~ }
// fun Group method-san() {}
// "#,
//     );
// }
//
// #[test]
// fn error_empty_contract_body() {
//     assert_error(
//         &"[EMPTY CONTRACT BODY]".red(),
//         r#"
// fun main-san() { pwint()~ }
// contwact Interface {}
// "#,
//     );
// }
//
// #[test]
// fn error_param_after_variadic() {
//     assert_error(
//         &format!(
//             "{} {}",
//             "[PARAMETER AFTER VARIADIC]".red(),
//             "at line 1 from column 38 to 41".bold(),
//         ),
//         r#"fun main-san(arg-chan, arg2-chan..., arg3-chan) {
//     pwint()~
// }"#,
//     );
// }
//
// #[test]
// fn error_invalid_global_token() {
//     let header = "[INVALID GLOBAL TOKEN]".red();
//     let asserter = |text: &str| assert_error(&header, text);
//     let invalids = vec![
//         "aqua", "Aqua", "chan", "kun", "senpai", "san", "sama", "dono", "+", "-", "*", "/", "%",
//         "+=", "-=", "*=", "/=", "%=", "<", "<=", ">=", ">", "and", "or", "not", "==", "!=", "=",
//         "(", ")", "[", "]", "{", "}", ".", "?", "!", "...", ",", ":", "#", "|", "~", "main",
//         "wetuwn", "iwf", "ewse", "ewif", "mash", "default", "fow", "bweak", "continue", "in", "1",
//         "1.1", "fax", "cap", "\"cap\"", "'a'", "nuww",
//     ];
//     for invalid in invalids {
//         asserter(invalid);
//     }
// }
//
// #[test]
// fn error_invalid_inner_data_type() {
//     let header = "[INVALID INNER DATA TYPE]".red();
//     let mut initial_text = "hi aqua-chan{".to_owned();
//     let mut asserter = |text: &str| {
//         assert_error(&header, {
//             initial_text.push_str(text);
//             &initial_text
//         });
//     };
//     let invalids = vec![
//         "aqua", "+", "-", "*", "/", "%", "+=", "-=", "*=", "/=", "%=", "<", "<=", ">=", ">", "and",
//         "or", "not", "==", "!=", "=", "(", ")", "[", "]", "{", "}", ".", "?", "!", "...", ",", ":",
//         "#", "|", "~", "main", "wetuwn", "iwf", "ewse", "ewif", "mash", "default", "fow", "bweak",
//         "continue", "in", "1", "1.1", "fax", "cap", "\"cap\"", "'a'", "nuww",
//     ];
//     for invalid in invalids {
//         asserter(invalid);
//     }
// }
//
// #[test]
// fn error_invalid_for_initialization() {
//     let header = "[INVALID FOW INITIALZATION]".red();
//     let mut initial_text = "fun main-san(){fow ".to_owned();
//     let mut asserter = |text: &str| {
//         assert_error(&header, {
//             initial_text.push_str(text);
//             &initial_text
//         });
//     };
//     let invalids = vec![
//         "Aqua", "chan", "kun", "senpai", "san", "sama", "dono", "+", "-", "*", "/", "%", "+=",
//         "-=", "*=", "/=", "%=", "<", "<=", ">=", ">", "and", "or", "not", "==", "!=", "=", "(",
//         ")", "[", "]", "{", "}", ".", "?", "!", "...", ",", ":", "#", "|", "~", "main", "fun",
//         "gwoup", "contwact", "wetuwn", "iwf", "ewse", "ewif", "mash", "default", "fow", "bweak",
//         "continue", "in", "1", "1.1", "fax", "cap", "\"cap\"", "'a'", "nuww",
//     ];
//     for invalid in invalids {
//         asserter(invalid);
//     }
// }
