// Copyright (c) 2021, James Tan Juan Whei
// All rights reserved.

// This source code is licensed under the BSD-style license found in the
// LICENSE file in the root directory of this source tree.

pub mod debug;
pub mod parser;
pub mod scanner;
pub mod token;

#[macro_export]
macro_rules! enum_str {
    (#[derive($($trait:ident),*)]
    pub enum $name:ident {
        $($variant:ident),*,
    }) => {
        #[derive($($trait),*)]
        pub enum $name {
            $($variant),*
        }

        impl $name {
            pub fn name(&self) -> &'static str {
                match self {
                    $($name::$variant => stringify!($variant)),*
                }
            }
        }
    };
}
