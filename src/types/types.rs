use crate::type_inference::DataType;

pub fn is_indexable(x: &DataType) -> bool {
    matches!(x, DataType::String | DataType::Array(_))
}
