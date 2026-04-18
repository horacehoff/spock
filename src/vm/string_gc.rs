use crate::Data;
use crate::vm::ArrayPool;
use crate::vm::StringPool;

pub fn string_gc(
    array_pool: &ArrayPool,
    string_pool: &StringPool,
    free_strings: &mut Vec<u16>,
    registers: &[Data],
    recursion_stack: &[Data],
) {
    let mut live = vec![false; string_pool.len()];
    for data in registers.iter().chain(recursion_stack.iter()) {
        if data.is_large_str() {
            live[data.get_str_pool_id()] = true;
        } else if data.is_array() {
            track_strings(array_pool, &array_pool[data.as_array()], &mut live);
        }
    }

    // Prevent duplicates: mark already-free slots as live
    for &id in free_strings.iter() {
        live[id as usize] = true;
    }

    for (i, s) in live.iter().enumerate() {
        if !s {
            free_strings.push(i as u16);
        }
    }
}

fn track_strings(array_pool: &ArrayPool, array: &[Data], live: &mut [bool]) {
    if array.is_empty() {
        return;
    }
    if array[0].is_large_str() {
        for x in array {
            live[x.get_str_pool_id()] = true;
        }
    } else if array[0].is_array() {
        for x in array {
            track_strings(array_pool, &array_pool[x.as_array()], live);
        }
    }
}
