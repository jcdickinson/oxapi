use std::cmp::Ordering;
use std::fmt;
use std::ops::{Deref, DerefMut};

/// A query parameter flag. Present means `true`, absent means `false`.
///
/// Deserializes from bare presence (`?flag` or `?flag=`), explicit `?flag=true`,
/// or `?flag=false`. Defaults to `false` when the parameter is absent.
///
/// Serializes as `""` when `true` and `"false"` when `false`.
/// Use `#[serde(skip_serializing_if = "Flag::is_unset")]` to omit when `false`.
#[derive(Clone, Copy, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct Flag(pub bool);

impl Flag {
    /// Returns `true` if the flag is set.
    pub fn is_set(&self) -> bool {
        self.0
    }

    /// Returns `true` if the flag is not set.
    /// For use with `#[serde(skip_serializing_if = "Flag::is_unset")]`.
    pub fn is_unset(&self) -> bool {
        !self.0
    }
}

impl fmt::Debug for Flag {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl fmt::Display for Flag {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl Deref for Flag {
    type Target = bool;

    fn deref(&self) -> &bool {
        &self.0
    }
}

impl DerefMut for Flag {
    fn deref_mut(&mut self) -> &mut bool {
        &mut self.0
    }
}

impl From<bool> for Flag {
    fn from(value: bool) -> Self {
        Flag(value)
    }
}

impl From<Flag> for bool {
    fn from(flag: Flag) -> Self {
        flag.0
    }
}

impl AsRef<bool> for Flag {
    fn as_ref(&self) -> &bool {
        &self.0
    }
}

impl AsMut<bool> for Flag {
    fn as_mut(&mut self) -> &mut bool {
        &mut self.0
    }
}

impl PartialEq<bool> for Flag {
    fn eq(&self, other: &bool) -> bool {
        self.0 == *other
    }
}

impl PartialOrd<bool> for Flag {
    fn partial_cmp(&self, other: &bool) -> Option<Ordering> {
        self.0.partial_cmp(other)
    }
}

impl serde::Serialize for Flag {
    fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        serializer.serialize_bool(self.0)
    }
}

impl<'de> serde::Deserialize<'de> for Flag {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        struct FlagVisitor;

        impl serde::de::Visitor<'_> for FlagVisitor {
            type Value = Flag;

            fn expecting(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                f.write_str("a boolean flag (empty, \"true\", or \"false\")")
            }

            fn visit_bool<E: serde::de::Error>(self, v: bool) -> Result<Flag, E> {
                Ok(Flag(v))
            }

            fn visit_str<E: serde::de::Error>(self, v: &str) -> Result<Flag, E> {
                match v {
                    "" | "true" => Ok(Flag(true)),
                    "false" => Ok(Flag(false)),
                    _ => Err(E::invalid_value(
                        serde::de::Unexpected::Str(v),
                        &self,
                    )),
                }
            }

            fn visit_unit<E: serde::de::Error>(self) -> Result<Flag, E> {
                Ok(Flag(true))
            }
        }

        deserializer.deserialize_any(FlagVisitor)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn default_is_false() {
        assert_eq!(Flag::default(), false);
    }

    #[test]
    fn deref() {
        let flag = Flag(true);
        let b: &bool = &flag;
        assert!(b);
    }

    #[test]
    fn deref_mut() {
        let mut flag = Flag(false);
        *flag = true;
        assert!(flag.is_set());
    }

    #[test]
    fn from_bool() {
        assert_eq!(Flag::from(true), true);
        assert_eq!(bool::from(Flag(false)), false);
    }

    #[test]
    fn ord_with_bool() {
        assert!(Flag(true) > false);
        assert!(Flag(false) < true);
        assert!(Flag(true) == true);
    }

    #[test]
    fn deserialize_empty_string() {
        let flag: Flag = serde_json::from_str(r#""""#).unwrap();
        assert!(flag.is_set());
    }

    #[test]
    fn deserialize_true_string() {
        let flag: Flag = serde_json::from_str(r#""true""#).unwrap();
        assert!(flag.is_set());
    }

    #[test]
    fn deserialize_false_string() {
        let flag: Flag = serde_json::from_str(r#""false""#).unwrap();
        assert!(flag.is_unset());
    }

    #[test]
    fn deserialize_bool() {
        let flag: Flag = serde_json::from_str("true").unwrap();
        assert!(flag.is_set());

        let flag: Flag = serde_json::from_str("false").unwrap();
        assert!(flag.is_unset());
    }

    #[test]
    fn deserialize_null_as_default() {
        let flag: Option<Flag> = serde_json::from_str("null").unwrap();
        assert!(flag.is_none());
    }

    #[test]
    fn serialize_true() {
        let json = serde_json::to_string(&Flag(true)).unwrap();
        assert_eq!(json, "true");
    }

    #[test]
    fn serialize_false() {
        let json = serde_json::to_string(&Flag(false)).unwrap();
        assert_eq!(json, "false");
    }

    #[test]
    fn skip_serializing_if() {
        #[derive(serde::Serialize)]
        struct Q {
            #[serde(skip_serializing_if = "Flag::is_unset")]
            idp: Flag,
        }

        let with = serde_json::to_string(&Q { idp: Flag(true) }).unwrap();
        assert_eq!(with, r#"{"idp":true}"#);

        let without = serde_json::to_string(&Q { idp: Flag(false) }).unwrap();
        assert_eq!(without, "{}");
    }

    #[test]
    fn query_string_round_trip() {
        #[derive(serde::Deserialize)]
        struct Q {
            #[serde(default)]
            idp: Flag,
        }

        // Simulates ?idp= (bare presence, axum deserializes as empty string)
        let q: Q = serde_urlencoded::from_str("idp=").unwrap();
        assert!(q.idp.is_set());

        // Simulates ?idp=true
        let q: Q = serde_urlencoded::from_str("idp=true").unwrap();
        assert!(q.idp.is_set());

        // Simulates absent param
        let q: Q = serde_urlencoded::from_str("").unwrap();
        assert!(q.idp.is_unset());
    }
}
