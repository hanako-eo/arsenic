use crate::FileId;

#[derive(Debug, Clone, Copy)]
pub struct Span {
    pub file_id: FileId,
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn new(file_id: FileId, start: usize, end: usize) -> Self {
        Self {
            file_id,
            start,
            end,
        }
    }
    pub(crate) fn from_pest(file_id: FileId, span: pest::Span<'_>) -> Self {
        Self::new(file_id, span.start(), span.end())
    }

    pub fn extent_right(self, other: Self) -> Self {
        debug_assert_eq!(self.file_id, other.file_id);
        debug_assert!(self.start <= other.start);
        debug_assert!(self.end <= other.end);

        Self::new(self.file_id, self.start, other.end)
    }
}
