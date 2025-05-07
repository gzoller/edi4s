package co.blocke.edi4j.table;

import java.util.List;

public interface TableRow {
    List<Cell> getCells();
    Style getStyle();
    Align getAlign();
}