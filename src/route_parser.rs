use staticinfrastructure::*;
use parser_utils::*;



//pub struct Route {
//    pub signal :ObjectId,
//    pub first_trigger :ObjectId,
//    pub sections: SmallVec<[ObjectId; 4]>,
//    pub switch_positions: SmallVec<[(ObjectId, SwitchPosition);2]>,
//    pub length: f64,
//    pub releases: SmallVec<[Release;2]>,
//}
//
//pub struct Release {
//    pub trigger :ObjectId,
//    pub resources :SmallVec<[ObjectId; 4]>,
//}


//
// LEXER
//
//
#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    BraceOpen, BraceClose, ListOpen, ListClose, ListSep,
    Number(f64),
    Identifier(String),
    EOF,
}

#[derive(Debug,Clone)]
pub enum LexerError {
    UnexpectedChar(usize, String),
    UnexpectedEOF,
}


use std::iter::Peekable;
pub fn lexer(x: &mut Iterator<Item = char>) -> Result<Vec<Token>, LexerError> {
    let mut tokens = Vec::new();
    let mut input = x.peekable();
    let mut line = 0;
    loop {
        match input.peek() {
            Some(&ch) => {
                match ch {
                    x if x.is_numeric() => {
                        let num: String = consume_while(&mut input, |a| {
                                a.is_numeric() || a == '-' || a == 'e' || a == 'E' || a == '.'
                            })
                            .into_iter()
                            .collect();
                        tokens.push(Token::Number(num.parse::<f64>().unwrap()));
                    }
                    x if x.is_alphabetic() => {
                        let s: String = consume_while(&mut input, |a| a.is_alphanumeric())
                            .into_iter()
                            .collect();
                        tokens.push(Token::Identifier(s));
                    }
                    '(' => {
                        input.next().unwrap();
                        tokens.push(Token::ListOpen);
                    }
                    ')' => {
                        input.next().unwrap();
                        tokens.push(Token::ListClose);
                    }
                    ',' => {
                        input.next().unwrap();
                        tokens.push(Token::ListSep);
                    }
                    '{' => {
                        input.next().unwrap();
                        tokens.push(Token::BraceOpen);
                    }
                    '}' => {
                        input.next().unwrap();
                        tokens.push(Token::BraceClose);
                    }
                    ' ' | '\r' | '\t' => {
                        input.next().unwrap();
                    }
                    '\n' => {
                        input.next().unwrap();
                        line += 1;
                    }
                    c => {
                        return Err(LexerError::UnexpectedChar(line, c.to_string()));
                    }
                }
            }
            None => {
                break;
            }
        }
    }
    tokens.push(Token::EOF);
    Ok(tokens)
}

