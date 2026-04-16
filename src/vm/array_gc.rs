use crate::ArrayStorage;
use crate::Data;

pub fn alloc_array(
    array_pool: &mut ArrayStorage,
    free_arrays: &mut Vec<u16>,
    registers: &[Data],
    recursion_stack: &[Data],
) -> u32 {
    if let Some(id) = free_arrays.pop() {
        array_pool[id as usize].clear();
        id as u32
    } else {
        // Only run the GC if the array pool is too big
        let id = array_pool.len() as u32;
        array_pool.push(Vec::new());
        if array_pool.len() > 100 {
            gc_arrays(array_pool, free_arrays, registers, recursion_stack);
        }
        id
    }
}

fn gc_arrays(
    array_pool: &ArrayStorage,
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

    // Mark as free any array that isn't referenced by a register
    for i in 0..array_pool.len() {
        if !live[i] {
            free_arrays.push(i as u16);
        }
    }
}

fn track_arrays(id: usize, array_pool: &ArrayStorage, live: &mut [bool]) {
    if live[id] {
        return;
    }
    live[id] = true;
    // Track nested arrays
    for elem in &array_pool[id] {
        if elem.is_array() {
            track_arrays(elem.as_array() as usize, array_pool, live);
        }
    }
}
