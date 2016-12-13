#[derive(Debug, PartialEq)]
pub enum EitherResult {
    First,
    Second,
}

/// A collection of miscellaneous methods for `&str`.
pub trait StrUtil {
    /// Returns a `String` between `delim_a` and `delim_b`.
    ///
    /// # Example
    ///
    /// ```
    /// use twirc::util::string::StrUtil;
    ///
    /// println!("{:?}", "a:selected_string!garbage".between(":", "!"));
    /// ```
    fn between(&self, delim_a: &str, delim_b: &str) -> Result<String, String>;

    /// Returns the portion of a `&str` that occurs after `delim`.
    ///
    /// # Example
    ///
    /// ```
    /// use twirc::util::string::StrUtil;
    ///
    /// println!("{:?}", ":garbage :selected_string".after(" :"));
    /// ```
    fn after(&self, delim: &str) -> Result<String, String>;

    /// Looks for `a` and `b` in a string and returns the first one that is there.
    ///
    /// # Example
    ///
    /// ```
    /// use twirc::util::string::{StrUtil, EitherResult};
    ///
    /// let x = "test +o test";
    /// let y = "test -o test";
    ///
    /// assert_eq!(x.either("+o", "-o"), Ok(EitherResult::First));
    /// assert_eq!(y.either("+o", "-o"), Ok(EitherResult::Second));
    /// ```
    fn either(&self, a: &str, b: &str) -> Result<EitherResult, String>;
}

impl StrUtil for str {
    fn between(&self, delim_a: &str, delim_b: &str) -> Result<String, String> {
        let start_idx =
            self
            .find(delim_a)
            .ok_or(format!("util::string::between(): start string \"{}\" not found", delim_a))?;

        let start_str = &self[start_idx + delim_a.len()..];

        let end =
            start_str
            .find(delim_b)
            .ok_or(format!("util::string::between(): end string \"{}\" not found", delim_b))?;

        Ok(start_str[..end].to_string())
    }

    fn after(&self, delim: &str) -> Result<String, String> {
        self
        .find(delim)
        .map(|i| self[i + delim.len()..].to_string())
        .ok_or(format!("util::string::after(): string delimiter \"{}\" not found", delim))
    }

    fn either(&self, a: &str, b: &str) -> Result<EitherResult, String> {
        self
        .find(a)
        .map(|_| EitherResult::First)
        .or_else(|| self.find(b).map(|_| EitherResult::Second))
        .ok_or(format!("util::string::either(): unable to find \"{}\" or \"{}\"", a, b))
    }
}