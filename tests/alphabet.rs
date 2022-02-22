use pgx::*;
use pgx_named_columns::*;

const ALPHABET: &str = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";

pub struct IndexedLetter {
    idx: i8,
    letter: char,
}

#[pg_extern_columns("tests/alphabet.rs")]
fn alphabet(length: i8) -> impl Iterator<Item = IndexedLetter> {
    ALPHABET
        .chars()
        .take(length.clamp(0, 25) as usize)
        .enumerate()
        .map(|(idx, letter)| IndexedLetter {
            idx: idx as _,
            letter,
        })
}

#[cfg(test)]
mod tests {
    #[test]
    fn compiles() {}
}
