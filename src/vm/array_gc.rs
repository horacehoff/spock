use crate::ArrayPool;
use crate::Data;

pub fn alloc_array(
    array_pool: &mut ArrayPool,
    free_arrays: &mut Vec<u16>,
    registers: &[Data],
    recursion_stack: &[Data],
) -> u32 {
    if let Some(id) = free_arrays.pop() {
        array_pool[id as usize].clear();
        id as u32
    } else {
        if array_pool.len() >= 100 {
            array_gc(array_pool, free_arrays, registers, recursion_stack);
        }
        if let Some(id) = free_arrays.pop() {
            array_pool[id as usize].clear();
            id as u32
        } else {
            let id = array_pool.len() as u32;
            array_pool.push(Vec::new());
            id
        }
    }
}

fn array_gc(
    array_pool: &ArrayPool,
    free_arrays: &mut Vec<u16>,
    registers: &[Data],
    recursion_stack: &[Data],
) {
    let mut live = vec![false; array_pool.len()];

    // Recursively find all "used" arrays
    for data in registers.iter().chain(recursion_stack.iter()) {
        if data.is_array() {
            track_arrays(data.as_array() as usize, array_pool, &mut live);
        }
    }

    // Prevent duplicates: mark already-free slots as live
    for &id in free_arrays.iter() {
        live[id as usize] = true;
    }

    // Mark as free any array that isn't referenced by a register
    for i in 0..array_pool.len() {
        if !live[i] {
            free_arrays.push(i as u16);
        }
    }
}

/// Tracks nested arrays
fn track_arrays(id: usize, array_pool: &ArrayPool, live: &mut [bool]) {
    if live[id] {
        return;
    }
    live[id] = true;
    // Arrays can only hold a single type
    // As such, if the first element in the array is not an array, then the other elements aren't either
    if array_pool[id].len() != 0 && !array_pool[id][0].is_array() {
        return;
    }
    for elem in &array_pool[id] {
        track_arrays(elem.as_array() as usize, array_pool, live);
    }
}
