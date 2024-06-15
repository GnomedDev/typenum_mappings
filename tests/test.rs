#![allow(dead_code)]

use arrayvec::ArrayString;

trait ArrayStringLike {}

impl<const N: usize> ArrayStringLike for ArrayString<N> {}

trait TypeNumToArrayString {
    type ArrayString: ArrayStringLike;
}

typenum_mappings::impl_typenum_mapping!(
    impl<const INDEX: usize = 0..=4> TypeNumToArrayString for #TypeNumName {
        type ArrayString = ArrayString<INDEX>;
    }
);

type ArrayString4 = <typenum::U<4> as TypeNumToArrayString>::ArrayString;
fn test() -> ArrayString<4> {
    ArrayString4::new()
}
