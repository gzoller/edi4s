package co.blocke.edi4j.table;

import java.util.*;
import java.util.stream.Collectors;
import java.util.Objects;

public class HtmlRenderer implements Renderer {
    private final Table table;

    public HtmlRenderer(Table table) {
        this.table = Objects.requireNonNull(table);
    }

    @Override
    public String render() {
        StringBuilder sb = new StringBuilder();
        sb.append("<!DOCTYPE html><html><head><meta charset=\"UTF-8\">\n");
        sb.append("<style>\n");
        sb.append("table { border-collapse: collapse; width: 100%; font-family: Arial, sans-serif; }\n");
        sb.append("th, td { border: 1px solid #ccc; padding: 6px 12px; vertical-align: top; }\n");
        sb.append("th { background-color: #003366; color: white; text-align: center; }\n");
        sb.append(".primary { background-color: #cce5ff; font-weight: bold; }\n");
        sb.append(".secondary { background-color: #e2e3e5; font-weight: bold; }\n");
        sb.append(".tertiary { background-color: #d4edda; color: #155724; }\n");
        sb.append(".neutral { background-color: #ffffff; }\n");
        sb.append(".muted { background-color: #f9f9f9; color: #666; }\n");
        sb.append(".alert { background-color: #f8d7da; color: #721c24; font-weight: bold; }\n");
        sb.append(".warn { background-color: #ffeeba; color: #856404; font-weight: bold; }\n");
        sb.append(".left { text-align: left; }\n");
        sb.append(".center { text-align: center; }\n");
        sb.append(".right { text-align: right; }\n");
        sb.append(".decimal { text-align: right; }\n");
        sb.append("</style></head><body>\n");

        for (Title t : table.getTitle()) {
            sb.append("<h3>")
                    .append(t.getCells().get(0).getContent())
                    .append("</h3>\n");
        }

        sb.append("<table>\n<thead>\n<tr>");
        for (Cell c : table.getHeader().getCells()) {
            String alignClass = effectiveAlign(c, table.getHeader().getAlign())
                    .toString().toLowerCase();
            sb.append("<th class='")
                    .append(alignClass)
                    .append("'>")
                    .append(c.getContent())
                    .append("</th>");
        }
        sb.append("</tr>\n</thead>\n<tbody>\n");

        for (BodyRow row : table.getRows()) {
            sb.append("<tr>");
            String rowStyle = row.getStyle().toString().toLowerCase();
            int totalCols = table.getHeader().getCells().size();
            int numCells   = row.getCells().size();

            List<Cell> cells = row.getCells();
            if (numCells < totalCols) {
                int collapseIdx = numCells - 1;
                int newColspan  = totalCols - collapseIdx;
                List<Cell> newCells = new ArrayList<>(cells);
                Cell last = cells.get(collapseIdx);
                newCells.set(collapseIdx,
                        new Cell(
                                last.getContent(),
                                last.getStyle(),
                                last.getAlign(),
                                last.getIndent(),
                                newColspan
                        )
                );
                cells = newCells;
            }

            for (Cell c : cells) {
                int colspan = c.getColspan();
                String colspanAttr = colspan > 1
                        ? " colspan='" + colspan + "'" : "";
                String indent = "";
                for (int k = 0; k < c.getIndent(); k++) {
                    indent += "&nbsp;&nbsp;";
                }
                String styleClass = c.getStyle()
                        .map(s -> s.toString().toLowerCase())
                        .orElse(rowStyle);
                String alignClass = effectiveAlign(c, row.getAlign())
                        .toString().toLowerCase();
                sb.append("<td class='")
                        .append(styleClass).append(" ")
                        .append(alignClass).append("'")
                        .append(colspanAttr).append(">")
                        .append(indent).append(c.getContent())
                        .append("</td>");
            }
            sb.append("</tr>\n");
        }

        sb.append("</tbody></table>\n</body></html>");
        return sb.toString();
    }

    @Override
    public List<Integer> normalizedColumnWidths() {
        int totalPct = table.getColumnWidthPct().stream()
                .mapToInt(Integer::intValue)
                .sum();
        return table.getColumnWidthPct().stream()
                .map(p -> (int) (p.doubleValue() / totalPct * table.getTableWidth()))
                .collect(Collectors.toList());
    }

    @Override
    public String effectiveStyle(Style style) {
        return style.toString().toLowerCase();
    }

    @Override
    public Align effectiveAlign(Cell cell, Align rowAlign) {
        return cell.getAlign().orElse(rowAlign);
    }
}