package co.blocke.edi4j.table;

import java.util.List;

public interface Renderer {
    String render();
    List<Integer> normalizedColumnWidths();
    String effectiveStyle(Style style);
    Align effectiveAlign(Cell cell, Align rowAlign);
}