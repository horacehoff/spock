// 51 bits of total payload -- 3 bits for data type => 48 bits of actual payload
const NAN_BASE: u64 =
    0b1111_1111_1111_1000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000;
const PAYLOAD_MASK: u64 = 0b1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111;
const NAN_TAG_BOOL: u64 = NAN_BASE | (1 << 48);
const NAN_TAG_STRING_SMALL: u64 = NAN_BASE | (2 << 48);
const NAN_TAG_STRING_LARGE: u64 = NAN_BASE | (3 << 48);
const NAN_TAG_ARRAY: u64 = NAN_BASE | (4 << 48);
const NAN_TAG_NULL: u64 = NAN_BASE | (5 << 48);
const NAN_TAG_FILE: u64 = NAN_BASE | (6 << 48);
const NAN_TAG_INT: u64 = NAN_BASE | (7 << 48);
const BOOL_TABLE: [Data; 2] = [Data::FALSE, Data::TRUE];

#[derive(Debug, Clone, Copy)]
pub struct Data(pub u64);

impl Data {
    pub const NULL: Data = Data(NAN_TAG_NULL);
    pub const FALSE: Data = Data(NAN_TAG_BOOL);
    pub const TRUE: Data = Data(NAN_TAG_BOOL | 1);
    #[inline(always)]
    pub fn is_null(&self) -> bool {
        self.0 == NAN_TAG_NULL
    }
    #[inline(always)]
    pub fn bool(b: bool) -> Data {
        Data(NAN_TAG_BOOL | b as u64)
    }
    #[inline(always)]
    pub fn as_bool(&self) -> bool {
        debug_assert!(self.is_bool());
        (self.0 & 1) != 0
    }
    #[inline(always)]
    pub fn is_bool(&self) -> bool {
        (self.0 & !PAYLOAD_MASK) == NAN_TAG_BOOL
    }
    #[inline(always)]
    pub fn float(n: f64) -> Data {
        Data(n.to_bits())
    }
    #[inline(always)]
    pub fn as_float(&self) -> f64 {
        debug_assert!(self.is_float());
        f64::from_bits(self.0)
    }
    #[inline(always)]
    pub fn is_float(&self) -> bool {
        (self.0 & NAN_BASE) != NAN_BASE
    }
    #[inline(always)]
    pub fn int(n: i32) -> Data {
        Data(NAN_TAG_INT | (n as u32 as u64))
    }
    #[inline(always)]
    pub fn as_int(&self) -> i32 {
        debug_assert!(self.is_int());
        self.0 as i32
    }
    #[inline(always)]
    pub fn is_int(&self) -> bool {
        (self.0 & !PAYLOAD_MASK) == NAN_TAG_INT
    }
    #[inline(always)]
    pub fn array(id: u32) -> Data {
        Data(NAN_TAG_ARRAY | id as u64)
    }
    #[inline(always)]
    pub fn as_array(&self) -> u32 {
        debug_assert!(self.is_array());
        (self.0 & PAYLOAD_MASK) as u32
    }
    #[inline(always)]
    pub fn is_array(&self) -> bool {
        (self.0 & !PAYLOAD_MASK) == NAN_TAG_ARRAY
    }
    #[inline(always)]
    pub fn str(s: &str) -> Data {
        if s.len() <= 5 {
            // Store it directly
            let mut payload: u64 = 0;
            for (i, &byte) in s.as_bytes().iter().enumerate() {
                payload |= (byte as u64) << (i * 8);
            }
            payload |= (s.len() as u64) << 40;
            Data(NAN_TAG_STRING_SMALL | payload)
        } else {
            panic!()
        }
    }
    #[inline(always)]
    pub fn as_str(&self) -> String {
        debug_assert!(self.is_str());
        if (self.0 & !PAYLOAD_MASK) == NAN_TAG_STRING_SMALL {
            let payload = self.0 & PAYLOAD_MASK;
            let len = (payload >> 40) as usize;
            let mut bytes = [0u8; 5];
            for (i, b) in bytes.iter_mut().enumerate().take(len) {
                *b = ((payload >> (i * 8)) & 0xFF) as u8;
            }
            str::from_utf8(&bytes[..len]).unwrap().to_string()
        } else {
            // LARGE STRING
            unreachable!("NOT A STRING");
        }
    }
    #[inline(always)]
    pub fn is_str(&self) -> bool {
        (self.0 & !PAYLOAD_MASK) == NAN_TAG_STRING_SMALL
            || (self.0 & !PAYLOAD_MASK) == NAN_TAG_STRING_LARGE
    }
    #[inline(always)]
    pub fn file(s: &str) -> Data {
        if s.len() <= 5 {
            // Store it directly
            let mut payload: u64 = 0;
            for (i, &byte) in s.as_bytes().iter().enumerate() {
                payload |= (byte as u64) << (i * 8);
            }
            payload |= (s.len() as u64) << 40;
            Data(NAN_TAG_FILE | payload)
        } else {
            panic!()
        }
    }
    #[inline(always)]
    pub fn as_file(&self) -> String {
        debug_assert!(self.is_file());
        if (self.0 & !PAYLOAD_MASK) == NAN_TAG_FILE {
            let payload = self.0 & PAYLOAD_MASK;
            let len = (payload >> 40) as usize;
            let mut bytes = [0u8; 5];
            for (i, b) in bytes.iter_mut().enumerate().take(len) {
                *b = ((payload >> (i * 8)) & 0xFF) as u8;
            }
            str::from_utf8(&bytes[..len]).unwrap().to_string()
        } else {
            unreachable!("NOT A FILE");
        }
    }
    #[inline(always)]
    pub fn is_file(&self) -> bool {
        (self.0 & !PAYLOAD_MASK) == NAN_TAG_FILE
    }
}

impl PartialEq for Data {
    #[inline(always)]
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

// f64 <=> DATA
impl From<f64> for Data {
    #[inline(always)]
    fn from(value: f64) -> Self {
        Data::float(value)
    }
}
impl From<Data> for f64 {
    #[inline(always)]
    fn from(value: Data) -> Self {
        value.as_float()
    }
}

// i64 <=> DATA
impl From<i32> for Data {
    #[inline(always)]
    fn from(value: i32) -> Self {
        Data::int(value)
    }
}
impl From<Data> for i32 {
    #[inline(always)]
    fn from(value: Data) -> Self {
        value.as_int()
    }
}

impl From<bool> for Data {
    #[inline(always)]
    fn from(value: bool) -> Self {
        BOOL_TABLE[value as usize]
    }
}
impl From<Data> for bool {
    #[inline(always)]
    fn from(value: Data) -> Self {
        value.as_bool()
    }
}
impl From<&str> for Data {
    #[inline(always)]
    fn from(value: &str) -> Self {
        Data::str(value)
    }
}
impl From<String> for Data {
    #[inline(always)]
    fn from(value: String) -> Self {
        Data::str(&value)
    }
}
impl From<Data> for String {
    #[inline(always)]
    fn from(value: Data) -> Self {
        value.as_str()
    }
}
