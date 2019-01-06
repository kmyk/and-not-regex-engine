use std::cmp::{max, min};

#[derive(Clone)]
#[derive(Eq)]
#[derive(PartialEq)]
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
        return CharClass {
            ranges: Vec::new(),
        };
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
        for (l2, r2) in stk.iter().rev() {
            self.push(*l2, *r2);
        }
    }

    /// O(n)
    pub fn union(&self, other: &CharClass) -> CharClass {
        let mut acc = CharClass::new();
        let mut it1 = self.ranges.iter().peekable();
        let mut it2 = other.ranges.iter().peekable();
        loop {
            match (it1.peek(), it2.peek()) {
                (Some((l1, r1)), Some((l2, r2))) => {
                    if l1 < l2 {
                        acc.push(*l1, *r1);
                        it1.next();
                    } else {
                        acc.push(*l2, *r2);
                        it2.next();
                    }
                }
                (Some((l1, r1)), None) => {
                    acc.push(*l1, *r1);
                    it1.next();
                }
                (None, Some((l2, r2))) => {
                    acc.push(*l2, *r2);
                    it2.next();
                }
                (None, None) => {
                    break;
                }
            }
        }
        return acc;
    }

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

    pub fn complement(&self) -> CharClass {
        let mut acc = CharClass::new();
        let mut last = 0;
        for (l, r) in &self.ranges {
            if last < *l {
                acc.push(last, *l);
            }
            last = *r;
        }
        if last < std::char::MAX as u32 {
            acc.push(last, std::char::MAX as u32);
        }
        return acc;
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
        for (l, r) in &self.ranges {
            if *l <= m && m < *r {
                return true;
            } else if m < *l {
                break;
            }
        }
        return false;
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

        } else if len < (std::char::MAX as usize) / 2 {
            /// TODO: fix a problem on '^', '-' and ']'
            let mut s = String::new();
            s.push('[');
            for (l, r) in &self.ranges {
                s.push(std::char::from_u32(*l).unwrap());
                if r - l >= 2 {
                    s.push('-');
                    s.push(std::char::from_u32(*r - 1).unwrap());
                }
            }
            s.push(']');
            return s;

        } else {
            /// TODO: fix a problem on '^', '-' and ']'
            let mut s = String::new();
            s.push_str("[^");
            for (l, r) in &self.complement().ranges {
                s.push(std::char::from_u32(*l).unwrap());
                if r - l >= 2 {
                    s.push('-');
                    s.push(std::char::from_u32(*r - 1).unwrap());
                }
            }
            s.push(']');
            return s;
        }
    }
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
}
