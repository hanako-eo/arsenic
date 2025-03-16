use crate::FileId;

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
            end
        }
    }
    pub(crate) fn from_pest(file_id: FileId, span: pest::Span<'_>) -> Self {
        Self::new(file_id, span.start(), span.end())
    }
}
