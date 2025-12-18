//! Serializer options for YAML emission.
//!
//! Controls indentation and optional anchor name generation for the serializer.
//!
//! Example: use 4-space indentation and a custom anchor naming scheme.
//!
//! ```rust
//! use serde::Serialize;
//!
//! #[derive(Serialize)]
//! struct Item { a: i32, b: bool }
//!
//! let mut buf = String::new();
//! let opts = serde_saphyr::SerializerOptions {
//!     indent_step: 4,
//!     anchor_generator: Some(|id| format!("id{}/", id)),
//!     ..Default::default()
//! };
//! serde_saphyr::to_fmt_writer_with_options(&mut buf, &Item { a: 1, b: true }, opts).unwrap();
//! assert!(buf.contains("a: 1"));
//! ```

use crate::ser_error::Error;

/// Controls the chomping behavior for block scalars.
///
/// In YAML block scalars (`|` or `>`), the chomp indicator determines how trailing
/// newlines are handled:
/// - `Strip` (`-`): Remove all trailing newlines from the content
/// - `Clip` (default): Keep exactly one trailing newline
/// - `Keep` (`+`): Preserve all trailing newlines
///
/// See [YAML 1.2 Spec - Chomping Indicator](https://yaml.org/spec/1.2.2/#8112-chomping-indicator)
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ChompIndicator {
    /// Strip (`-`): Remove all trailing newlines (e.g., `|-` or `>-`)
    Strip,
    /// Clip (no indicator): Keep exactly one trailing newline (e.g., `|` or `>`)
    Clip,
    /// Keep (`+`): Preserve all trailing newlines (e.g., `|+` or `>+`)
    Keep,
}

#[derive(Clone, Copy)]
pub struct SerializerOptions {
    /// Number of spaces to indent per nesting level when emitting block-style collections.
    pub indent_step: usize,
    /// Number of spaces to indent array items (sequences). If `None`, uses `indent_step`.
    /// This allows reproducing Go's YAML behavior where arrays are indented 2 spaces
    /// while objects are indented 4 spaces.
    pub indent_array: Option<usize>,
    /// Optional custom anchor-name generator.
    ///
    /// Receives a monotonically increasing `usize` id (starting at 1) and returns the
    /// anchor name to emit. If `None`, the built-in generator yields names like `a1`, `a2`, ...
    pub anchor_generator: Option<fn(usize) -> String>,
    /// Threshold for block-string wrappers ([crate::LitStr]/[crate::FoldStr] and owned variants
    /// [crate::LitString]/[crate::FoldString]).
    ///
    /// If the string contains a newline, block style is always used. Otherwise, when the
    /// string is single-line and its length is strictly less than this threshold, the
    /// serializer emits a normal YAML scalar (no block style). Longer strings use block
    /// styles `|` or `>` depending on the wrapper. See the type docs for
    /// [crate::LitStr], [crate::FoldStr], [crate::LitString] and [crate::FoldString] for
    /// examples.
    pub min_fold_chars: usize,
    /// Maximum width (in characters) for lines in folded block scalars (`>`).
    ///
    /// Lines are wrapped at whitespace so that each emitted line is at most this many
    /// characters long (excluding indentation). If no whitespace is present within the
    /// limit, a hard break is performed.
    pub folded_wrap_chars: usize,
    /// When enabled, serialize simple enums that become a single scalar (unit variants)
    /// using YAML tags, e.g. `!!Enum Variant` instead of a plain scalar `Variant`.
    /// Deserializer does not need this setting as both cases will be understood.
    pub tagged_enums: bool,
    /// When enabled, strings containing newlines are automatically serialized using
    /// YAML literal block scalar style (`|`) instead of quoted strings with escape sequences.
    /// This matches Go's yaml.v3 behavior.
    ///
    /// Default: `false` (preserves backwards compatibility)
    pub prefer_block_scalars: bool,
    /// When enabled, empty maps are serialized as `{}` instead of being left empty/null.
    /// This matches Go's yaml.v3 behavior.
    ///
    /// Default: `false` (preserves backwards compatibility)
    pub empty_map_as_braces: bool,
    /// When enabled, empty arrays/sequences are serialized as `[]` instead of being left empty.
    /// This matches Go's yaml.v3 behavior.
    ///
    /// Default: `false` (preserves backwards compatibility)
    pub empty_array_as_brackets: bool,
    /// Maximum line width for automatic line wrapping.
    ///
    /// When set to `Some(width)`, long string values that exceed the specified width
    /// (after accounting for indentation) will be automatically wrapped using YAML
    /// folded block scalar style (`>`). This matches Go's yaml.v3 behavior with
    /// `encoder.SetWidth(80)`.
    ///
    /// - `None`: No automatic wrapping (default, preserves backwards compatibility)
    /// - `Some(80)`: Wrap lines at 80 characters (common default, matches Go yaml.v3)
    ///
    /// Note: This only affects block-style output. Flow-style contexts (inline
    /// sequences/maps) do not support block scalars and will use quoted strings.
    pub line_width: Option<usize>,
    /// When enabled, large floating-point numbers may be serialized using scientific
    /// notation (e.g., `1e+11` instead of `100000000000`).
    ///
    /// When disabled, floats are always serialized in plain decimal notation.
    /// This can be useful when the output needs to be consumed by parsers that
    /// don't handle scientific notation well.
    ///
    /// Default: `true` (preserves backwards compatibility)
    pub use_scientific_notation: bool,
    /// Controls the chomp indicator for block scalars.
    ///
    /// - `None` (default): Auto-detect based on trailing newlines (current behavior)
    ///   - 0 trailing newlines → `|-` (strip)
    ///   - 1 trailing newline → `|` (clip)
    ///   - 2+ trailing newlines → `|+` (keep)
    /// - `Some(ChompIndicator::Strip)`: Always use `|-` (matches Go yaml.v3 for strings without trailing newlines)
    /// - `Some(ChompIndicator::Clip)`: Always use `|`
    /// - `Some(ChompIndicator::Keep)`: Always use `|+`
    ///
    /// When a fixed chomp indicator is specified, the content is adjusted to ensure
    /// round-trip correctness. For example, with `Strip`, trailing newlines are removed
    /// from the content before emitting.
    pub block_scalar_chomp: Option<ChompIndicator>,
    /// When enabled, string keys that look like numbers (e.g., "12345", "77387")
    /// are quoted in the YAML output. This matches Go's yaml.v3 behavior.
    ///
    /// Default: `false` (preserves backwards compatibility)
    pub quote_numeric_strings: bool,
}

// Below this length, block-string wrappers serialize as regular scalars
// instead of YAML block styles. This keeps short values compact.
pub(crate) const MIN_FOLD_CHARS: usize = 32;
/// Maximum width (in characters) for lines inside folded block scalars.
/// Lines will be wrapped at whitespace so that each emitted line is at most
/// this many characters long (excluding indentation). If no whitespace is
/// available within the limit, a hard break is performed.
pub(crate) const FOLDED_WRAP_CHARS: usize = 80;

impl SerializerOptions {
    pub(crate) fn consistent(&self) -> Result<(), Error> {
        if self.indent_step == 0 {
            return Err(Error::InvalidOptions(
                "Invalid indent step must be positive".to_string(),
            ));
        }
        Ok(())
    }
}

impl Default for SerializerOptions {
    fn default() -> Self {
        // Defaults mirror internal constants used by the serializer.
        Self {
            indent_step: 2,
            indent_array: None,
            anchor_generator: None,
            min_fold_chars: MIN_FOLD_CHARS,
            folded_wrap_chars: FOLDED_WRAP_CHARS,
            tagged_enums: false,
            prefer_block_scalars: false,
            empty_map_as_braces: false,
            empty_array_as_brackets: false,
            line_width: None,
            use_scientific_notation: true,
            block_scalar_chomp: None,
            quote_numeric_strings: false,
        }
    }
}
