use std::cmp::{max, min};
use std::collections::HashSet;
use std::mem::swap;
use std::str::Chars;

#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct CharClass {
    ranges: Vec<(u32, u32)>,
}

impl CharClass {
    /// O(1) amortized
    /// The argument is a half-open range [l, r).
    fn push(&mut self, l1: u32, r1: u32) {
        assert!(l1 < r1);
        match self.ranges.last().cloned() {
            None => {
                self.ranges.push((l1, r1));
            }
            Some((l2, r2)) => {
                assert!(l2 <= l1);
                if l1 <= r2 {
                    self.ranges.pop();
                    self.ranges.push((l2, max(r1, r2)));
                } else {
                    self.ranges.push((l1, r1));
                }
            }
        }
    }

    /// O(1)
    pub fn new() -> CharClass {
        return CharClass { ranges: Vec::new() };
    }

    /// O(1)
    pub fn from_char(c: char) -> CharClass {
        let mut cls = CharClass::new();
        cls.insert(c);
        return cls;
    }

    /// O(n)
    pub fn insert(&mut self, value: char) {
        self.insert_range(value, value);
    }

    /// O(n)
    /// The argument is a closed range [l, r].
    pub fn insert_range(&mut self, l: char, r: char) {
        let l1 = l as u32;
        let r1 = r as u32 + 1;
        let mut stk = Vec::new();
        loop {
            match self.ranges.last().cloned() {
                Some((l2, r2)) if l1 < l2 => {
                    self.ranges.pop();
                    stk.push((l2, r2));
                }
                _ => {
                    break;
                }
            }
        }
        self.push(l1, r1);
        for (l2, r2) in stk.iter().rev().cloned() {
            self.push(l2, r2);
        }
    }

    /// O(n)
    pub fn union(&self, other: &CharClass) -> CharClass {
        let mut acc = CharClass::new();
        let mut it1 = self.ranges.iter().peekable();
        let mut it2 = other.ranges.iter().peekable();
        loop {
            match (it1.peek().cloned().cloned(), it2.peek().cloned().cloned()) {
                (Some((l1, r1)), Some((l2, r2))) => {
                    if l1 < l2 {
                        acc.push(l1, r1);
                        it1.next();
                    } else {
                        acc.push(l2, r2);
                        it2.next();
                    }
                }
                (Some((l1, r1)), None) => {
                    acc.push(l1, r1);
                    it1.next();
                }
                (None, Some((l2, r2))) => {
                    acc.push(l2, r2);
                    it2.next();
                }
                (None, None) => {
                    break;
                }
            }
        }
        return acc;
    }

    /// O(n)
    pub fn intersection(&self, other: &CharClass) -> CharClass {
        let mut acc = CharClass::new();
        let mut it1 = self.ranges.iter().peekable();
        let mut it2 = other.ranges.iter().peekable();
        let mut rng1: Option<(u32, u32)> = None;
        let mut rng2: Option<(u32, u32)> = None;
        loop {
            if rng1.is_none() {
                rng1 = it1.next().cloned();
            }
            if rng2.is_none() {
                rng2 = it2.next().cloned();
            }
            match (rng1, rng2) {
                (Some((l1, r1)), Some((l2, r2))) => {
                    let l = max(l1, l2);
                    let r = min(r1, r2);
                    if l < r {
                        acc.push(l, r);
                    }
                    rng1 = if r < r1 { Some((max(r, l1), r1)) } else { None };
                    rng2 = if r < r2 { Some((max(r, l2), r2)) } else { None };
                }
                _ => {
                    break;
                }
            }
        }
        return acc;
    }

    /// O(n)
    pub fn complement(&self) -> CharClass {
        let mut acc = CharClass::new();
        let mut last = 0;
        for (l, r) in self.ranges.iter().cloned() {
            if last < l {
                acc.push(last, l);
            }
            last = r;
        }
        if last < std::char::MAX as u32 {
            acc.push(last, std::char::MAX as u32);
        }
        return acc;
    }

    /// O(n)
    pub fn subtract(&self, other: &CharClass) -> CharClass {
        return self.intersection(&other.complement());
    }

    /// O(n)
    pub fn len(&self) -> usize {
        let mut acc = 0;
        for (l, r) in &self.ranges {
            acc += r - l;
        }
        return acc as usize;
    }

    /// O(1)
    pub fn is_empty(&self) -> bool {
        return self.ranges.is_empty();
    }

    /// O(n)
    pub fn contains(&self, value: char) -> bool {
        let m = value as u32;
        for (l, r) in self.ranges.iter().cloned() {
            if l <= m && m < r {
                return true;
            } else if m < l {
                break;
            }
        }
        return false;
    }

    // TODO: return as an Iter<(char, char)>
    pub fn iter(&self) -> Vec<(char, char)> {
        return self
            .ranges
            .iter()
            .map(|(l, r)| {
                (
                    std::char::from_u32(*l).unwrap(),
                    std::char::from_u32(*r - 1).unwrap(),
                )
            })
            .collect();
    }
}

impl ToString for CharClass {
    fn to_string(&self) -> String {
        let len = self.len();

        if len == 0 {
            return "".to_string();
        } else if len == 1 {
            let (l, _) = self.ranges.first().unwrap();
            let l = std::char::from_u32(*l).unwrap();
            if l == '*' || l == '[' || l == '\\' {
                return "\\".to_string() + &l.to_string();
            } else {
                return l.to_string();
            }
        } else if len == std::char::MAX as usize {
            return ".".to_string();
        } else {
            let is_complement = len > (std::char::MAX as usize) / 2;
            let mut has_close = false;
            let mut has_hat = false;
            let mut has_hyphen = false;
            let mut touch = |c: u32| -> bool {
                if c == '[' as u32 {
                    has_close = true;
                } else if c == '^' as u32 {
                    has_hat = true;
                } else if c == '-' as u32 {
                    has_hyphen = true;
                } else {
                    return false;
                }
                return true;
            };
            let mut s = String::new();
            let complement = self.complement();
            let iter = (if is_complement {
                complement.ranges.iter()
            } else {
                self.ranges.iter()
            })
            .cloned();
            for (l, r) in iter {
                let mut l = l;
                let mut r = r;
                if touch(l) {
                    l += 1;
                }
                if l == r {
                    continue;
                }
                if touch(r - 1) {
                    r -= 1;
                }
                if l == r {
                    continue;
                }
                let len = r - l;
                let l = std::char::from_u32(l).unwrap();
                let r = std::char::from_u32(r - 1).unwrap();
                s.push(l);
                if len >= 3 {
                    s.push('-');
                }
                if len >= 2 {
                    s.push(r);
                }
            }
            if s.is_empty() && !has_close && has_hat && has_hyphen {
                return "[-^]".to_string();
            }
            let mut t = String::new();
            t.push('[');
            if is_complement {
                t.push('^');
            }
            if has_close {
                t.push(']');
            }
            t.push_str(&s);
            if has_hat {
                t.push('^');
            }
            if has_hyphen {
                t.push('-');
            }
            t.push(']');
            return t;
        }
    }
}

/// assume '[' is read and ignore remaining chars
pub fn parse_char_class(chars: &mut Chars) -> Result<CharClass, &'static str> {
    let mut is_complemented = false;
    let mut s = Vec::new();
    match chars.next() {
        None => {
            return Err("Unmatched [");
        }
        Some('^') => {
            is_complemented = true;
            match chars.next() {
                None => {
                    return Err("Unmatched [^");
                }
                Some(c) => {
                    s.push(c);
                }
            }
        }
        Some(c) => {
            s.push(c);
        }
    }
    loop {
        match chars.next() {
            None => {
                return Err("Unmatched [ or [^");
            }
            Some(']') => {
                break;
            }
            Some(c) => {
                s.push(c);
            }
        }
    }
    let mut cls = CharClass::new();
    let mut i: usize = 0;
    while i < s.len() {
        if i + 2 < s.len() && s[i + 1] == '-' {
            if s[i] > s[i + 2] {
                return Err("Invalid range end");
            } else {
                cls.insert_range(s[i], s[i + 2]);
                i += 3;
            }
        } else {
            cls.insert(s[i]);
            i += 1;
        }
    }
    if is_complemented {
        cls = cls.complement();
    }
    return Ok(cls);
}

pub fn discriminate(clss: &Vec<CharClass>) -> HashSet<CharClass> {
    let mut cur = HashSet::new();
    let mut prv = HashSet::new();
    for cls in clss.iter() {
        if cur.is_empty() {
            cur.insert(cls.clone());
        } else {
            swap(&mut cur, &mut prv);
            cur.clear();
            let cls_comp = cls.complement();
            for it in &prv {
                cur.insert(it.intersection(&cls));
                cur.insert(it.intersection(&cls_comp));
            }
        }
    }
    cur.remove(&CharClass::new());
    return cur;
}

#[test]
fn it_works() {
    let mut a = CharClass::new();

    a.insert_range('a', 'c');
    assert_eq!(a.to_string(), "[a-c]");
    assert_eq!(a.ranges.len(), 1);
    assert_eq!(a.len(), 3);

    a.insert('f');
    assert_eq!(a.to_string(), "[a-cf]");
    assert_eq!(a.ranges.len(), 2);
    assert_eq!(a.len(), 4);

    a.insert_range('x', 'z');
    assert_eq!(a.to_string(), "[a-cfx-z]");
    assert_eq!(a.ranges.len(), 3);
    assert_eq!(a.len(), 7);
    assert!(a.contains('c'));
    assert!(!a.contains('d'));
    assert!(a.contains('f'));
    assert!(a.contains('z'));

    let b = a.complement();
    assert_eq!(b.to_string(), "[^a-cfx-z]");
    assert!(!b.contains('c'));
    assert!(b.contains('d'));
    assert!(!b.contains('f'));
    assert!(!b.contains('z'));
    assert!(b.intersection(&a).is_empty());
    assert!(b.union(&a).complement().is_empty());

    a.insert_range('b', 'y');
    assert_eq!(a.to_string(), "[a-z]");
    assert_eq!(a.ranges.len(), 1);
    assert_eq!(a.len(), 26);

    a.insert('^');
    assert_eq!(a.to_string(), "[a-z^]");
    assert_eq!(a.complement().to_string(), "[^a-z^]");

    a.insert(']');
    assert_eq!(a.to_string(), "[]a-z^]");
    assert_eq!(a.complement().to_string(), "[^]a-z^]");

    a.insert('-');
    assert_eq!(a.to_string(), "[]a-z^-]");
    assert_eq!(a.complement().to_string(), "[^]a-z^-]");
}
