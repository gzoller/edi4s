package co.blocke.edi4j.table;

import java.util.Objects;
import java.util.List;

public class Header implements TableRow {
    private final List<Cell> cells;
    private final Style style;
    private final Align align;

    public Header(List<Cell> cells) {
        this(cells, Style.PRIMARY, Align.CENTER);
    }

    public Header(List<Cell> cells, Style style, Align align) {
        this.cells = Objects.requireNonNull(cells);
        this.style = Objects.requireNonNull(style);
        this.align = Objects.requireNonNull(align);
    }

    @Override public List<Cell> getCells() { return cells; }
    @Override public Style getStyle() { return style; }
    @Override public Align getAlign() { return align; }
}