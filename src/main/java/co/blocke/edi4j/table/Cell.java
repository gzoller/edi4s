package co.blocke.edi4j.table;

import java.util.Optional;
import java.util.Objects;

public class Cell {
    private final String content;
    private final Optional<Style> style;
    private final Optional<Align> align;
    private final int indent;
    private final int colspan;

    /** original one‐arg constructor */
    public Cell(String content) {
        this(content, Optional.empty(), Optional.empty(), 0, 1);
    }

    /** two‐arg convenience: content + style */
    public Cell(String content, Optional<Style> style) {
        this(content, style, Optional.empty(), 0, 1);
    }

    /** three‐arg convenience: content + style + align */
    public Cell(String content, Optional<Style> style, Optional<Align> align) {
        this(content, style, align, 0, 1);
    }

    /** full constructor */
    public Cell(String content,
                Optional<Style> style,
                Optional<Align> align,
                int indent,
                int colspan) {
        this.content = Objects.requireNonNull(content);
        this.style   = style   != null ? style   : Optional.empty();
        this.align   = align   != null ? align   : Optional.empty();
        this.indent  = indent;
        this.colspan = colspan;
    }

    public String getContent()      { return content; }
    public Optional<Style> getStyle()   { return style; }
    public Optional<Align> getAlign()   { return align; }
    public int getIndent()         { return indent; }
    public int getColspan()        { return colspan; }
}
