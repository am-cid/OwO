use crate::{
    lexer::{lexer::Lexer, token::TokenKind},
    parser::{parser::Parser, productions::Production},
};

#[test]
fn hello_world() {
    let source = r#"fun main-san(){pwint("hello world")~}"#;
    let formatted = r#"
fun main-san() {
    pwint("hello world")~
}"#
    .trim();
    assert_formatted_eq(source, formatted);
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
    assert_formatted_eq(source, formatted);
    assert_already_formatted(formatted);
}

#[test]
fn declaration_and_assignment() {
    let source = r#"
    fun main-san() {
        hi aqua-chan=1~hi aqua-kun=2.0~
        hi aqua-sama=fax~hi aqua-senpai="string"~
        hi aqua-kouhai='c'~hi aqua-san=nuww~
        hi aqua-dono=any(1,2,3,)~hi aqua-Class=Class(4,5,6,)~
        hi aqua-chan=6~aqua=7~
        aqua.hands=8~aqua.scream().pitch=9~
    }"#;
    let formatted = r#"
fun main-san() {
    hi aqua-chan = 1~
    hi aqua-kun = 2.0~
    hi aqua-sama = fax~
    hi aqua-senpai = "string"~
    hi aqua-kouhai = 'c'~
    hi aqua-san = nuww~
    hi aqua-dono = any(1, 2, 3)~
    hi aqua-Class = Class(4, 5, 6)~
    hi aqua-chan = 6~
    aqua = 7~
    aqua.hands = 8~
    aqua.scream().pitch = 9~
}"#
    .trim();
    assert_formatted_eq(source, formatted);
    assert_already_formatted(formatted);
}

#[test]
fn optional_types() {
    let source = r#"
fun main-san() {
    hi aqua-chan?=nuww~hi aqua-chan!?=1~
    hi aqua-kun[1]?=nuww~hi aqua-kun[1]!?=[2.0,]~
    hi aqua-sama{}?=nuww~hi aqua-sama{}!?=#[fax,]~
    hi aqua-senpai{kouhai}?=nuww~hi aqua-senpai{kouhai}!?=#["string": 's',]~
    hi aqua-dono{san}?=nuww~hi aqua-dono{san}!?=#[any(1,2,3,): nuww,]~
    hi aqua-Class?=nuww~hi aqua-Class!?=Class(4,5,6,)~
}
fun Sample! mutable_method-san(other-chan){uwu.property+=other~}"#;
    let formatted = r#"
fun main-san() {
    hi aqua-chan? = nuww~
    hi aqua-chan!? = 1~
    hi aqua-kun[1]? = nuww~
    hi aqua-kun[1]!? = [2.0]~
    hi aqua-sama{}? = nuww~
    hi aqua-sama{}!? = #[fax]~
    hi aqua-senpai{kouhai}? = nuww~
    hi aqua-senpai{kouhai}!? = #["string": 's']~
    hi aqua-dono{san}? = nuww~
    hi aqua-dono{san}!? = #[any(1, 2, 3): nuww]~
    hi aqua-Class? = nuww~
    hi aqua-Class!? = Class(4, 5, 6)~
}
fun Sample! mutable_method-san(other-chan) {
    uwu.property += other~
}"#
    .trim();
    assert_formatted_eq(source, formatted);
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
    assert_formatted_eq(source, formatted);
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
    assert_formatted_eq(source, formatted);
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
    assert_formatted_eq(source, formatted);
    assert_already_formatted(formatted);
}

#[test]
fn group_and_contract_definition() {
    let source = r#"fun main-san(){pwint()~}
    gwoup HololiveMember[]{name-senpai~age-kun~manager-senpai~}
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
    assert_formatted_eq(source, formatted);
    assert_already_formatted(formatted);
}

#[test]
fn group_contract_implementations() {
    let source = r#"fun main-san() {pwint()~}gwoup Empty[]{id-chan~}gwoup HololiveMember[Idol,Streamer,]{name-senpai~age-kun~manager-senpai~}contwact Idol {sing-senpai(chan, kun,)~dance-senpai(kouhai, sama,)~}contwact Streamer{go_live-sama()~farm_clips-senpai(chan,senpai,)~}"#;
    let formatted = r#"
fun main-san() {
    pwint()~
}
gwoup Empty {
    id-chan~
}
gwoup HololiveMember [Idol, Streamer] {
    name-senpai~
    age-kun~
    manager-senpai~
}
contwact Idol {
    sing-senpai(chan, kun)~
    dance-senpai(kouhai, sama)~
}
contwact Streamer {
    go_live-sama()~
    farm_clips-senpai(chan, senpai)~
}"#
    .trim();
    assert_formatted_eq(source, formatted);
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
    aqua = -3^4+5*-6~

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
    hi lap-sama = -1+-2>=-3^4==5 or 6 and not 7~
}"#
    .trim();
    let formatted = r#"
fun main-san() {
    hi aqua-chan = ((1 + 2) - 3)~
    aqua = (5 - (6 * 7))~
    aqua = ((8 * (9 + 1)) / 2)~
    aqua = (-(3 ^ 4) + (5 * -6))~
    hi ojou-dono = ((1 > 2) < 3)~
    ojou = ((4 >= 5) <= 6)~
    ojou = ((7 == 8) != 9)~
    hi shion-sama = ((1 or 2) or 3)~
    shion = (4 or (5 and 6))~
    shion = ((7 and 8) or 9)~
    shion = ((1 and 2) and 3)~
    shion = ((1 and 2) or (3 and 4))~
    shion = ((not 5 and 6) or 7)~
    hi lap-sama = ((((-1 + -2) >= -(3 ^ 4)) == 5) or (6 and not 7))~
}"#
    .trim();
    assert_formatted_eq(source, formatted)
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
    assert_formatted_eq(source, formatted);
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
    assert_formatted_eq(source, formatted);
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
    assert_formatted_eq(source, formatted);
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
    fow hi i-chan! = 1~ (i <= 3)~ (i + 1) {
        pwint()~
    }
    fow hi a-senpai! = ""~ (a == "owo")~ inpwt("input owo: ") {
        pwint()~
    }
}"#
    .trim();
    assert_formatted_eq(source, formatted);
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
    assert_formatted_eq(source, formatted);
    assert_already_formatted(formatted);
}

#[test]
fn function_pipelines() {
    let source = r#"
fun main-san() {aqua|pow(4)|pwint()~totally_random_vec_of_nums()|filter_evens()|sort_ascending()
|pwint("placeholder so the pipeline becomes long enough")~}"#;
    let formatted = r#"
fun main-san() {
    aqua | pow(4) | pwint()~
    totally_random_vec_of_nums()
    | filter_evens()
    | sort_ascending()
    | pwint("placeholder so the pipeline becomes long enough")~
}"#
    .trim();
    assert_formatted_eq(source, formatted);
    assert_already_formatted(formatted);
}

/// asserts that passed in formatted source is equal to how the parser formats it
fn assert_already_formatted(formatted: &'static str) -> () {
    assert_formatted_eq(formatted, formatted)
}
/// asserts that source, which will be formatted by parser, is equal to the passed in formatted
/// string
fn assert_formatted_eq(source: &'static str, formatted: &'static str) -> () {
    let mut lexer = Lexer::new(source);
    lexer.tokenize();
    let mut parser = Parser::new(
        source,
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
        parser.program.string(0),
        "expected:\n{}\n===\nactual:\n{}",
        formatted,
        parser.program.string(0),
    )
}
