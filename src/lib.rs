use logos::Logos;
use std::collections::HashMap;

#[derive(Logos, Debug, PartialEq, Clone)]
#[logos(skip r"[ \t\n\r]+")]
enum Token<'a> {
    #[token("<")]
    OpenTag,

    #[token(">")]
    CloseTag,

    #[token("</")]
    EndTagStart,

    #[regex(r"[a-zA-Z0-9_]+=", priority = 1, callback = |lex| lex.slice())]
    Key(&'a str),

    #[regex(r"#.*", priority = 1, callback = |lex| lex.slice())]
    Comment(&'a str),

    #[regex(r"[^\s<>=]+", priority = 2, callback = |lex| lex.slice())]
    Value(&'a str),
}

#[derive(Debug, Clone, PartialEq, Default)]
pub enum ConfigNode {
    Section(HashMap<String, ConfigNode>),
    Value(String),
    #[default]
    Empty,
}

impl ConfigNode {
    pub fn get(&self, path: &str) -> Option<&str> {
        let path = path.trim_start_matches('/');
        if path.is_empty() {
            return None;
        }

        let parts: Vec<&str> = path.split('/').collect();
        self.get_by_parts(&parts)
    }

    fn get_by_parts(&self, parts: &[&str]) -> Option<&str> {
        if parts.is_empty() {
            return None;
        }

        match self {
            ConfigNode::Section(map) => {
                if parts.len() == 1 {
                    if let Some(ConfigNode::Value(value)) = map.get(parts[0]) {
                        return Some(value);
                    }
                } else {
                    if let Some(node) = map.get(parts[0]) {
                        return node.get_by_parts(&parts[1..]);
                    }
                }
                None
            }
            ConfigNode::Value(_) => None,
            ConfigNode::Empty => None,
        }
    }
}

struct ConfigParser<'a> {
    tokens: Vec<Token<'a>>,
    position: usize,
}

impl<'a> ConfigParser<'a> {
    fn new(input: &'a str) -> Self {
        let lexer = Token::lexer(input);
        let tokens: Vec<Token<'a>> = lexer.filter_map(|result| result.ok()).collect();

        Self {
            tokens,
            position: 0,
        }
    }

    fn current_token(&self) -> Option<&Token<'a>> {
        self.tokens.get(self.position)
    }

    fn next_token(&mut self) {
        if self.position < self.tokens.len() {
            self.position += 1;
        }
    }

    fn expect_token(&mut self, expected: &Token) -> Result<(), String> {
        match self.current_token() {
            Some(token) if std::mem::discriminant(token) == std::mem::discriminant(expected) => {
                self.next_token();
                Ok(())
            }
            Some(token) => Err(format!("Expected {:?}, got {:?}", expected, token)),
            None => Err("Unexpected end of input".to_string()),
        }
    }

    fn parse_identifier(&mut self) -> Result<&'a str, String> {
        match self.current_token() {
            Some(Token::Value(ident)) => {
                let name = *ident;
                self.next_token();
                Ok(name)
            }
            _ => Err("Expected identifier".to_string()),
        }
    }

    fn parse_key_value(&mut self) -> Result<(&'a str, &'a str), String> {
        match self.current_token() {
            Some(Token::Key(key)) => {
                let key_str = key.trim_end_matches('=');
                self.next_token();

                let value = match self.current_token() {
                    Some(Token::Value(val)) => *val,
                    _ => return Err("Expected value after key".to_string()),
                };
                self.next_token();

                Ok((key_str, value))
            }
            _ => Err("Expected key-value pair".to_string()),
        }
    }

    fn parse_section(&mut self) -> Result<HashMap<String, ConfigNode>, String> {
        self.expect_token(&Token::OpenTag)?;
        let section_name = self.parse_identifier()?.to_string();
        self.expect_token(&Token::CloseTag)?;

        let mut children = HashMap::new();

        while let Some(token) = self.current_token() {
            match token {
                Token::EndTagStart => {
                    break;
                }
                Token::OpenTag => {
                    let subsection_content = self.parse_section()?;
                    if let Some((subsection_name, subsection_node)) =
                        subsection_content.into_iter().next()
                    {
                        children.insert(subsection_name, subsection_node);
                    }
                }
                Token::Key(_) => {
                    let (key, value) = self.parse_key_value()?;
                    children.insert(key.to_string(), ConfigNode::Value(value.to_string()));
                }
                Token::Comment(_) => {
                    self.next_token();
                }
                _ => {
                    self.next_token();
                }
            }
        }

        self.expect_token(&Token::EndTagStart)?;
        let end_section_name = self.parse_identifier()?;
        if end_section_name != section_name {
            return Err(format!(
                "Mismatched section tags: start {}, end {}",
                section_name, end_section_name
            ));
        }
        self.expect_token(&Token::CloseTag)?;

        let mut section_content = HashMap::new();
        section_content.insert(section_name, ConfigNode::Section(children));
        Ok(section_content)
    }

    fn parse(&mut self) -> Result<ConfigNode, String> {
        let mut root_children = HashMap::new();

        while let Some(token) = self.current_token() {
            match token {
                Token::OpenTag => {
                    let section_content = self.parse_section()?;
                    for (section_name, section_node) in section_content {
                        root_children.insert(section_name, section_node);
                    }
                }
                Token::Comment(_) => {
                    self.next_token();
                }
                Token::Key(_) => {
                    let (key, value) = self.parse_key_value()?;
                    root_children.insert(key.to_string(), ConfigNode::Value(value.to_string()));
                }
                _ => break,
            }
        }

        if self.position < self.tokens.len() {
            return Err("Unexpected tokens remaining after parsing".to_string());
        }

        Ok(ConfigNode::Section(root_children))
    }
}

pub fn parse_config(config_str: &str) -> Result<ConfigNode, String> {
    let mut parser = ConfigParser::new(config_str);
    parser.parse()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_sample_config() {
        let config_str = r#"
<section_a>
    <section_b>
        a=b
        c=d
        e=f
        <section_c>
            g=h
        </section_c>
    </section_b>
</section_a>
"#;

        let result = parse_config(config_str);
        assert!(result.is_ok());

        let config = result.unwrap();
        assert_eq!(config.get("/section_a/section_b/a"), Some("b"));
        assert_eq!(config.get("/section_a/section_b/c"), Some("d"));
        assert_eq!(config.get("/section_a/section_b/e"), Some("f"));
        assert_eq!(config.get("/section_a/section_b/section_c/g"), Some("h"));
    }

    #[test]
    fn test_multiple_root_sections() {
        let config_str = r#"
<section_a>
    <section_b>
        a=b
        c=d
    </section_b>
</section_a>
<section_c>
        e=f
        g=h
</section_c>
<section_d>
        i=j
        k=l
</section_d>
"#;

        let result = parse_config(config_str);
        assert!(result.is_ok());

        let config = result.unwrap();
        assert_eq!(config.get("/section_a/section_b/a"), Some("b"));
        assert_eq!(config.get("/section_a/section_b/c"), Some("d"));
        assert_eq!(config.get("/section_c/e"), Some("f"));
        assert_eq!(config.get("/section_c/g"), Some("h"));
        assert_eq!(config.get("/section_d/i"), Some("j"));
        assert_eq!(config.get("/section_d/k"), Some("l"));
    }

    #[test]
    fn test_complex_config_structure() {
        let config_str = r#"
<section_a>
    a=b
    c=d
    <section_b>
        e=f
        g=h
        <section_c>
            i=j
            k=l
        </section_c>
    </section_b>
    <section_d>
        m=n
        o=p
    </section_d>
</section_a>
"#;

        let result = parse_config(config_str);
        assert!(result.is_ok());

        let config = result.unwrap();
        assert_eq!(config.get("/section_a/a"), Some("b"));
        assert_eq!(config.get("/section_a/c"), Some("d"));
        assert_eq!(config.get("/section_a/section_b/e"), Some("f"));
        assert_eq!(config.get("/section_a/section_b/g"), Some("h"));
        assert_eq!(
            config.get("/section_a/section_b/section_c/i"),
            Some("j")
        );
        assert_eq!(
            config.get("/section_a/section_b/section_c/k"),
            Some("l")
        );
        assert_eq!(config.get("/section_a/section_d/m"), Some("n"));
        assert_eq!(config.get("/section_a/section_d/o"), Some("p"));
    }

    #[test]
    fn test_get_method() {
        let config_str = r#"
<section_a>
    <section_b>
        a=b
        <section_c>
            c=d
        </section_c>
    </section_b>
</section_a>
"#;

        let config = parse_config(config_str).unwrap();

        assert_eq!(config.get("/section_a/section_b/a"), Some("b"));
        assert_eq!(config.get("section_a/section_b/a"), Some("b"));
        assert_eq!(config.get("/section_a/section_b/section_c/c"), Some("d"));
        assert_eq!(config.get("/section_a/section_b/NonExistent"), None);
        assert_eq!(config.get("/NonExistent/section_b/a"), None);
    }
}
