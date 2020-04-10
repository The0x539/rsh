use std::env;

extern crate itertools;
use itertools::Itertools;

#[derive(Debug, Clone)]
enum Error {
    BackslashAtEnd,
    UnbalancedQuotes,
    MultiRedirect,
    Expected(&'static str),
    Unexpected(&'static str),
    ParseFail,
}

type Result<T> = std::result::Result<T, Error>;

#[derive(Debug, Clone)]
enum Stdin {
    Terminal,
    File(String),
    Pipe,
}

#[derive(Debug, Clone)]
enum Token {
    Args(Vec<String>),
    Pipe,
    LeftWaka,
    RightWaka,
    DoubleRightWaka,
    And,
    Or,
    Semicolon,
    Ampersand,
}

#[derive(Debug, Clone)]
enum Stdout {
    Terminal,
    File(String),
    FileAppend(String),
    Pipe(Box<Command>),
}

#[derive(Debug, Clone)]
struct Command {
    argv: Vec<String>,
    stdin: Stdin,
    stdout: Stdout,
}

#[derive(Debug, Clone)]
enum JobCons {
    And(Box<Command>),  // &&
    Or(Box<Command>),   // ||
}

#[derive(Debug, Clone)]
struct Job {
    first: Command,
    rest: Vec<JobCons>,
    background: bool,
}

impl Command {
    pub fn set_stdin(&mut self, stdin: Stdin) -> Result<()> {
        match self.stdin {
            Stdin::Terminal => {
                self.stdin = stdin;
                Ok(())
            },
            _ => Err(Error::MultiRedirect)
        }
    }
    pub fn set_stdout(&mut self, stdout: Stdout) -> Result<()> {
        match self.stdout {
            Stdout::Terminal => {
                self.stdout = stdout;
                Ok(())
            },
            _ => Err(Error::MultiRedirect)
        }
    }
}

fn parse_token(cmdline: &str) -> Result<(Token, &str)> {
    let (token, rest) = None
        .or(parse_delimiter(cmdline))
        .or(parse_argument(cmdline, false)?.map(|(x, y)| (Token::Args(x), y)))
        .ok_or(Error::ParseFail)?;

    Ok((token, rest.trim_start()))
}

fn parse_delimiter(cmdline: &str) -> Option<(Token, &str)> {
    let symbols = [
        (">>", Token::DoubleRightWaka),
        (">", Token::RightWaka),
        ("<", Token::LeftWaka),
        ("||", Token::Or),
        ("|", Token::Pipe),
        ("&&", Token::And),
        ("&", Token::Ampersand),
        (";", Token::Semicolon),
    ];

    for (sym, tok) in &symbols {
        if cmdline.starts_with(sym) {
            return Some((tok.clone(), &cmdline[sym.len()..]));
        }
    }

    None
}

fn parse_quoted_str(cmdline: &str) -> Result<Option<(String, &str)>> {
    let double_quoted = match cmdline.chars().next() {
        Some('"') => true,
        Some('\'') => false,
        _ => return Ok(None),
    };

    let mut escaped = false;
    let mut contents = String::new();
    for (i, c) in cmdline.char_indices().skip(1) {
        if escaped {
            escaped = false;
            contents.push(c);
            continue;
        }
        let j = i + c.len_utf8();
        match c {
            '"' | '\'' => {
                if double_quoted == (c == '"') {
                    return Ok(Some((contents, &cmdline[j..])));
                } else {
                    contents.push(c);
                }
            },
            '\\' => escaped = true,
            '$' => {
                if double_quoted {
                    unimplemented!("$variable expansion inside quotes");
                } else {
                    contents.push(c);
                }
            },
            _ => contents.push(c),
        }
    }

    Err(Error::UnbalancedQuotes)
}

fn parse_unquoted_args_str(cmdline: &str) -> Result<Option<(String, &str)>> {
    let mut idx = 0;
    let mut escaped = false;
    let mut contents = String::new();
    for (i, c) in cmdline.char_indices() {
        if escaped {
            escaped = false;
            contents.push(c);
            idx = i + c.len_utf8();
            continue;
        }
        match c {
            '|' | '&' | ';' | '>' | '<' | '{' | '"' | '\'' | ' ' => break,
            '}' => return Err(Error::Unexpected("Closing brace at top level")),
            '\\' => escaped = true,
            '$' => {
                unimplemented!("$variable expansion outside quotes");
            },
            _ => contents.push(c),
        }
        idx = i + c.len_utf8();
    }

    if escaped {
        Err(Error::BackslashAtEnd)
    } else if contents.is_empty() {
        Ok(None)
    } else {
        Ok(Some((contents, &cmdline[idx..])))
    }
}

fn parse_unquoted_brace_list_str(cmdline: &str) -> Result<Option<(String, &str)>> {
    let mut idx = 0;
    let mut escaped = false;
    let mut contents = String::new();
    for (i, c) in cmdline.char_indices() {
        if escaped {
            escaped = false;
            contents.push(c);
            idx = i + c.len_utf8();
            continue;
        }
        match c {
            '{' | '"' | '\'' | ',' | '}' => break,
            '\\' => escaped = true,
            '$' => {
                unimplemented!("$variable expansion outside quotes (in brace list)");
            },
            _ => contents.push(c),
        }
        idx = i + c.len_utf8();
    }

    if escaped {
        Err(Error::BackslashAtEnd)
    } else if contents.is_empty() {
        Ok(None)
    } else {
        Ok(Some((contents, &cmdline[idx..])))
    }
}

fn parse_brace_list(mut cmdline: &str) -> Result<Option<(Vec<String>, &str)>> {
    let mut iter = cmdline.char_indices();
    match iter.next() {
        Some((_, '{')) => (),
        _ => return Ok(None),
    }

    let mut entries = Vec::new();
    let mut prev = '{';

    while let Some((i, c)) = iter.next() {
        let j = i + c.len_utf8();
        match c {
            ',' | '}' => {
                if prev == '{' || prev == ',' {
                    entries.push(String::new());
                }
                if c == '}' {
                    return Ok(Some((entries, &cmdline[j..])));
                }
            }
            _ => {
                let (pieces, rest) = parse_argument(&cmdline[i..], true)?
                    .ok_or(Error::Expected("Brace list entry"))?;

                entries.extend(pieces);
                cmdline = rest;
                iter = cmdline.char_indices();
            }
        }
        prev = c;
    }

    Err(Error::Expected("Closing brace"))
}

fn expand_argument(pieces: Vec<Vec<String>>) -> Vec<String> {
    pieces
        .into_iter()
        .multi_cartesian_product()
        .map(|x| x.join(""))
        .collect()
}

fn parse_argument(mut cmdline: &str, in_braces: bool) -> Result<Option<(Vec<String>, &str)>> {
    let mut arg = Vec::new();

    while !cmdline.is_empty() {

        let parse_unquoted_str = if in_braces {
            parse_unquoted_brace_list_str
        } else {
            parse_unquoted_args_str
        };

        let parse_result = None
            .or(parse_brace_list(cmdline)?)
            .or(parse_quoted_str(cmdline)?.map(|(x, y)| (vec![x], y)))
            .or(parse_unquoted_str(cmdline)?.map(|(x, y)| (vec![x], y)));

        let (piece, rest) = match parse_result {
            Some(x) => x,
            None => break,
        };

        arg.push(piece);
        cmdline = rest;
    }

    Ok(Some((expand_argument(arg), cmdline)))
}

fn parse_pipeline(mut cmdline: &str, stdin: Stdin) -> Result<(Command, &str)> {
    cmdline = cmdline.trim_start();

    let mut cmd = Command {
        argv: Vec::new(),
        stdin: stdin,
        stdout: Stdout::Terminal,
    };

    while !cmdline.is_empty() {
        let (token, rest) = parse_token(cmdline)?;
        match token {
            Token::Args(args) => cmd.argv.extend(args),
            Token::LeftWaka | Token::RightWaka | Token::DoubleRightWaka => {
                let (target, rest2) = parse_token(rest)?;
                match target {
                    Token::Args(args) => {
                        let arg = if args.len() == 1 {
                            args[0].clone()
                        } else {
                            return Err(Error::Expected("Single-valued redirection target"));
                        };
                        match token {
                            Token::LeftWaka => cmd.set_stdin(Stdin::File(arg))?,
                            Token::RightWaka => cmd.set_stdout(Stdout::File(arg))?,
                            Token::DoubleRightWaka => cmd.set_stdout(Stdout::FileAppend(arg))?,
                            _ => unreachable!("redirecting stdio using {:?}", token),
                        }
                    },
                    _ => return Err(Error::Expected("redirection target")),
                }
                cmdline = rest2;
                continue;
            },
            Token::Pipe => {
                 let (dest, rest2) = parse_pipeline(rest, Stdin::Pipe)?;
                 cmd.set_stdout(Stdout::Pipe(Box::new(dest)))?;
                 cmdline = rest2;
                 break;
            },
            Token::And | Token::Or | Token::Semicolon | Token::Ampersand => break,
        }
        cmdline = rest;
    }

    Ok((cmd, cmdline))
}

fn parse_job(mut cmdline: &str) -> Result<(Job, &str)> {
    cmdline = cmdline.trim_start();

    let (first_cmd, rest) = parse_pipeline(cmdline, Stdin::Terminal)?;
    cmdline = rest;

    let mut job = Job {
        first: first_cmd,
        rest: Vec::new(),
        background: false,
    };

    while !cmdline.is_empty() {
        let (token, rest) = parse_token(cmdline)?;
        match token {
            Token::And | Token::Or => {
                let (next_cmd, rest2) = parse_pipeline(rest, Stdin::Terminal)?;
                let cons_type = match token {
                    Token::And => JobCons::And,
                    Token::Or => JobCons::Or,
                    _ => unreachable!("chaining commands using {:?}", token),
                };
                job.rest.push(cons_type(Box::new(next_cmd)));
                cmdline = rest2;
            },
            Token::Ampersand | Token::Semicolon => {
                job.background = match token {
                    Token::Ampersand => true,
                    _ => false,
                };
                cmdline = rest;
                break;
            }
            _ => return Err(Error::Expected("pipeline separator or job separator")),
        }
    }

    Ok((job, cmdline))
}

fn parse_cmdline(mut cmdline: &str) -> Result<Vec<Job>> {
    let mut jobs = Vec::new();

    while !cmdline.is_empty() {
        let (job, rest) = parse_job(cmdline)?;
        jobs.push(job);
        cmdline = rest;
    }

    Ok(jobs)
}

fn main() {
    for arg in env::args().skip(1) {
        println!("{:?}", parse_cmdline(&arg));
    }
}
