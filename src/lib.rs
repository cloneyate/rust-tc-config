use logos::Logos;
use std::collections::HashMap;

#[derive(Logos, Debug, PartialEq, Clone)]
#[logos(skip r"[ \t\r]+")] // 现在只跳过空格和制表符，保留换行符作为Token
enum Token<'a> {
    #[token("<")]
    OpenTag,

    #[token(">")]
    CloseTag,

    #[token("</")]
    EndTagStart,

    #[token("=")]
    Equals,

    #[token("\n")]
    Newline,

    #[regex(r"#.*", priority = 1, callback = |lex| lex.slice())]
    Comment(&'a str),

    // 文本内容，需要包含至少一个非空白字符，避免与skip冲突
    #[regex(r"[^<>=\n\s]+", priority = 2, callback = |lex| lex.slice())]
    Text(&'a str),
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
        let node = self.get_node(path);
        match node {
            Some(ConfigNode::Value(value)) => Some(value),
            _ => None,
        }
    }

    pub fn get_items(&self, path: &str) -> Option<HashMap<String, String>> {
        let node = self.get_node(path);
        match node {
            Some(ConfigNode::Section(map)) => {
                let mut r = HashMap::new();
                for (k, v) in map.iter() {
                    if let ConfigNode::Value(value) = v {
                        r.insert(k.clone(), value.clone());
                    }
                }
                Some(r)
            }
            _ => None,
        }
    }

    pub fn get_node(&self, path: &str) -> Option<&ConfigNode> {
        let path = path.trim_start_matches('/');
        if path.is_empty() {
            return None;
        }

        let parts: Vec<&str> = path.split('/').collect();
        self.get_node_by_parts(&parts)
    }

    fn get_node_by_parts(&self, parts: &[&str]) -> Option<&ConfigNode> {
        if parts.is_empty() {
            return None;
        }

        match self {
            ConfigNode::Section(map) => {
                if parts.len() == 1 {
                    return map.get(parts[0]);
                } else {
                    if let Some(node) = map.get(parts[0]) {
                        return node.get_node_by_parts(&parts[1..]);
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

    fn parse_text(&mut self) -> Result<&'a str, String> {
        match self.current_token() {
            Some(Token::Text(ident)) => {
                let name = *ident;
                self.next_token();
                Ok(name)
            }
            _ => Err("Expected Token Text".to_string()),
        }
    }

    fn parse_key_value(&mut self) -> Result<(&'a str, String), String> {
        // 解析键名
        let key = match self.current_token() {
            Some(Token::Text(ident)) => {
                let key_str = *ident;
                self.next_token();
                key_str
            }
            _ => return Err("Expected identifier for key".to_string()),
        };

        // 解析等号
        self.expect_token(&Token::Equals)?;

        // 收集所有的Text和Equals作为值，直到遇到换行符
        let mut value_parts = Vec::new();
        loop {
            match self.current_token() {
                Some(Token::Text(val)) => {
                    value_parts.push(*val);
                    self.next_token();
                }
                Some(Token::Equals) => {
                    value_parts.push("=");
                    self.next_token();
                }
                Some(Token::Newline) => {
                    break;
                }
                _ => {
                    return Err("Expected text value or equals or newline after equals".to_string());
                }
            }
        }

        // 解析换行符
        self.expect_token(&Token::Newline)?;

        // 组合所有部分为值
        let value = value_parts.join("").trim().to_string();
        Ok((key, value))
    }

    fn parse_section(&mut self) -> Result<HashMap<String, ConfigNode>, String> {
        self.expect_token(&Token::OpenTag)?;
        let section_name = self.parse_text()?.to_string();
        self.expect_token(&Token::CloseTag)?;

        let mut children = HashMap::new();

        while let Some(token) = self.current_token() {
            match token {
                Token::EndTagStart => {
                    break;
                }
                Token::OpenTag => {
                    let subsection_content = self.parse_section()?;
                    for (subsection_name, subsection_node) in subsection_content {
                        // 如果已存在同名subsection，则忽略后来的subsection
                        if !children.contains_key(&subsection_name) {
                            children.insert(subsection_name, subsection_node);
                        }
                    }
                }
                Token::Text(_) => {
                    let (key, value) = self.parse_key_value()?;
                    children.insert(key.to_string(), ConfigNode::Value(value));
                }
                Token::Comment(_) => {
                    self.next_token();
                }
                Token::Newline => {
                    self.next_token();
                }
                _ => {
                    self.next_token();
                }
            }
        }

        self.expect_token(&Token::EndTagStart)?;
        let end_section_name = self.parse_text()?;
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
                        // 如果已存在同名section，则忽略后来的section
                        if !root_children.contains_key(&section_name) {
                            root_children.insert(section_name, section_node);
                        }
                    }
                }
                Token::Comment(_) => {
                    self.next_token();
                }
                Token::Text(_) => {
                    let (key, value) = self.parse_key_value()?;
                    root_children.insert(key.to_string(), ConfigNode::Value(value));
                }
                Token::Newline => {
                    self.next_token();
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
        if let Err(e) = &result {
            println!("Parse error: {}", e);
        }
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
        if let Err(e) = &result {
            println!("Parse error: {}", e);
        }
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
    a = b
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
        if let Err(e) = &result {
            println!("Parse error: {}", e);
        }
        assert!(result.is_ok());

        let config = result.unwrap();
        assert_eq!(config.get("/section_a/a"), Some("b"));
        assert_eq!(config.get("/section_a/c"), Some("d"));
        assert_eq!(config.get("/section_a/section_b/e"), Some("f"));
        assert_eq!(config.get("/section_a/section_b/g"), Some("h"));
        assert_eq!(config.get("/section_a/section_b/section_c/i"), Some("j"));
        assert_eq!(config.get("/section_a/section_b/section_c/k"), Some("l"));
        assert_eq!(config.get("/section_a/section_d/m"), Some("n"));
        assert_eq!(config.get("/section_a/section_d/o"), Some("p"));

        let section_a_items = config.get_items("/section_a").unwrap();
        assert_eq!(section_a_items.len(), 2);
        assert_eq!(section_a_items["a"], "b");
        assert_eq!(section_a_items["c"], "d");
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

    #[test]
    fn test_key_value_with_spaces() {
        let config_str = r#"
<section_a>
    key_with_spaces = value_with_spaces
    <section_b>
        key = value
        another_key   =   another_value  
    </section_b>
</section_a>
"#;

        let result = parse_config(config_str);
        if let Err(e) = &result {
            println!("Parse error: {}", e);
        }
        assert!(result.is_ok());

        let config = result.unwrap();
        assert_eq!(
            config.get("/section_a/key_with_spaces"),
            Some("value_with_spaces")
        );
        assert_eq!(config.get("/section_a/section_b/key"), Some("value"));
        assert_eq!(
            config.get("/section_a/section_b/another_key"),
            Some("another_value")
        );
    }
}
