package co.blocke.edi4j.table;

import java.util.Objects;
import java.util.List;

public class Row implements BodyRow {
    private final List<Cell> cells;
    private final Style style;
    private final Align align;

    public Row(List<Cell> cells) {
        this(cells, Style.NEUTRAL, Align.LEFT);
    }

    public Row(List<Cell> cells, Style style, Align align) {
        this.cells = Objects.requireNonNull(cells);
        this.style = Objects.requireNonNull(style);
        this.align = Objects.requireNonNull(align);
    }

    @Override public List<Cell> getCells() { return cells; }
    @Override public Style getStyle() { return style; }
    @Override public Align getAlign() { return align; }
}