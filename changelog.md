# unreleased

* Add `Data.Text.Utf8.Lines` and `Data.Text.Utf8.Rope` modules for ropes indexed by UTF-8 code units.
* Add UTF-8 indexing functionality to `Data.Text.Utf16.Rope.Mixed`.
* The metrics stored internally in the rope nodes has changed, which should improve performance by making some re-measuring redundant. As a consequence, the time complexity of `Data.Text.Rope.lengthAsPosition` is now linear in the length of the last line.

# 0.2

* Share `TextLines` between `Char` and UTF-16 modules.
* Add `Data.Text.Utf16.Rope.Mixed`.

# 0.1

* Initial release.
