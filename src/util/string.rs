pub trait StringUtil {
    fn between(&self, delim_a: &str, delim_b: &str) -> Result<String, String>;
    fn after(&self, delim: &str) -> Result<String, String>;
}

impl StringUtil for str {
    fn between(&self, delim_a: &str, delim_b: &str) -> Result<String, String> {
        let start_idx =
            self
            .find(delim_a)
            .ok_or(format!("between(): start string \"{}\" not found", delim_a))?;

        let start_str = &self[start_idx + delim_a.len()..];

        let end =
            start_str
            .find(delim_b)
            .ok_or(format!("between(): end string \"{}\" not found", delim_b))?;

        Ok(start_str[..end].to_string())
    }

    fn after(&self, delim: &str) -> Result<String, String> {
        self
        .find(delim)
        .map(|i| self[i + delim.len()..].to_string())
        .ok_or(format!("after(): string delimiter \"{}\" not found", delim))
    }
}