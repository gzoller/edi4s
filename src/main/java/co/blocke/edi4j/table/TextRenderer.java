package co.blocke.edi4j.table;

import java.util.*;
import java.util.AbstractMap.SimpleEntry;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import java.util.Objects;

public class TextRenderer implements Renderer {
    private static final String RESET = "\u001b[0m";
    private final Table table;

    public TextRenderer(Table table) {
        this.table = Objects.requireNonNull(table);
    }

    @Override
    public String render() {
        StringBuilder sb = new StringBuilder();
        List<Integer> widths = normalizedColumnWidths();
        // Compute decimal offsets map for quick lookup
        Map<Integer, Integer> offsetMap = findDecimalAlignedCells().stream()
                .collect(Collectors.toMap(SimpleEntry::getKey, SimpleEntry::getValue));

        // render title lines
        for (Title t : table.getTitle()) {
            sb.append(renderTitle(t, widths));
        }
        sb.append(renderDivider(widths, '='));

        // header
        sb.append(renderRow(table.getHeader(), widths, offsetMap));
        sb.append(renderDivider(widths, '='));

        // body rows
        List<BodyRow> rows = table.getRows();
        for (int i = 0; i < rows.size(); i++) {
            BodyRow r = rows.get(i);
            if (r instanceof SubHeader) {
                if (i > 0 && !(rows.get(i - 1) instanceof SubHeader)) {
                    sb.append(renderDivider(widths));
                }
                sb.append(renderRow(r, widths, offsetMap));
                sb.append(renderDivider(widths));
            } else { // Row
                sb.append(renderRow(r, widths, offsetMap));
                if (i == rows.size() - 1) {
                    sb.append(renderDivider(widths));
                }
            }
        }

        return sb.toString();
    }

    private List<SimpleEntry<Integer, Integer>> findDecimalAlignedCells() {
        List<TableRow> all = new ArrayList<>();
        all.add(table.getHeader());
        all.addAll(table.getRows());
        int cols = table.getColumnWidthPct().size();
        List<SimpleEntry<Integer, Integer>> result = new ArrayList<>();

        for (int col = 0; col < cols; col++) {
            int maxRight = 0;
            for (TableRow row : all) {
                if (col < row.getCells().size()) {
                    Cell cell = row.getCells().get(col);
                    Align a = effectiveAlign(cell, row.getAlign());
                    if (a == Align.DECIMAL) {
                        String c = cell.getContent().trim();
                        String[] parts = c.split("\\.");
                        if (parts.length == 2) {
                            maxRight = Math.max(maxRight, parts[1].length());
                        }
                    }
                }
            }
            if (maxRight > 0) {
                result.add(new SimpleEntry<>(col, maxRight));
            }
        }
        return result;
    }

    @Override
    public List<Integer> normalizedColumnWidths() {
        int totalPct = table.getColumnWidthPct().stream().mapToInt(Integer::intValue).sum();
        return table.getColumnWidthPct().stream()
                .map(p -> (int) (p.doubleValue() / totalPct * table.getTableWidth()))
                .collect(Collectors.toList());
    }

    @Override
    public String effectiveStyle(Style style) {
        return table.getStyleMap()
                .getOrDefault("text", Collections.emptyMap())
                .getOrDefault(style, "");
    }

    @Override
    public Align effectiveAlign(Cell cell, Align rowAlign) {
        return cell.getAlign().orElse(rowAlign);
    }

    private String renderTitle(Title title, List<Integer> widths) {
        int total = widths.stream().mapToInt(Integer::intValue).sum() + widths.size() + 1;
        String content = title.getCells().get(0).getContent();
        int pad = (total - content.length()) / 2;
        return " ".repeat(Math.max(0, pad)) + content + "\n";
    }

    private String renderDivider(List<Integer> widths) {
        return renderDivider(widths, '-');
    }

    private String renderDivider(List<Integer> widths, char ch) {
        String line = widths.stream()
                .map(w -> "+" + String.valueOf(ch).repeat(w))
                .collect(Collectors.joining());
        return line + "+\n";
    }

    private String padDecimal(String content, int width, int rightOffset) {
        String s = content.trim();
        String[] parts = s.split("\\.");
        String intPart = parts[0];
        String fracPart = parts.length == 2 ? parts[1] : "";
        String withDot = fracPart.isEmpty() ? intPart : intPart + "." + fracPart;

        String truncated;
        if (withDot.length() > width) {
            int cut = width - 3;
            truncated = cut <= 0 ? "..." : withDot.substring(0, cut) + "...";
        } else {
            truncated = withDot;
        }

        int totalLen = truncated.length();
        int leftPad = width - Math.max(totalLen, rightOffset + intPart.length() + (fracPart.isEmpty() ? 0 : 1));
        int rightPad = width - (leftPad + totalLen);

        return " ".repeat(Math.max(0, leftPad))
                + truncated
                + " ".repeat(Math.max(0, rightPad));
    }

    private String pad(String content, int width, Align align) {
        String t = content.length() > width
                ? content.substring(0, Math.max(0, width - 3)) + "..."
                : content;
        int len = t.length();
        switch (align) {
            case LEFT:
                return t + " ".repeat(Math.max(0, width - len));
            case CENTER: {
                int padL = (width - len) / 2;
                int padR = width - len - padL;
                return " ".repeat(Math.max(0, padL))
                        + t
                        + " ".repeat(Math.max(0, padR));
            }
            case RIGHT:
                return " ".repeat(Math.max(0, width - len)) + t;
            default: // DECIMAL fallback
                return pad(t, width, Align.RIGHT);
        }
    }

    private String renderRow(TableRow row,
                             List<Integer> widths,
                             Map<Integer, Integer> offsetMap) {
        StringBuilder sb = new StringBuilder();
        sb.append("|");

        int totalCols = widths.size();
        int cellCount = row.getCells().size();
        boolean collapsed = cellCount < totalCols;

        int colIdx = 0;
        for (int i = 0; i < cellCount; i++) {
            Cell cell = row.getCells().get(i);
            boolean lastCollapsed = collapsed && i == cellCount - 1;
            int span = lastCollapsed
                    ? totalCols - colIdx
                    : Math.max(cell.getColspan(), 1);

            int target = 0;
            for (int j = colIdx; j < colIdx + span; j++) target += widths.get(j);
            target += (span - 1);

            String styleStr = cell.getStyle()
                    .map(this::effectiveStyle)
                    .orElse(effectiveStyle(row.getStyle()));
            Align align = effectiveAlign(cell, row.getAlign());
            String content = " ".repeat(cell.getIndent() * 2) + cell.getContent();

            Integer offset = offsetMap.get(colIdx);
            String padded;
            if (align == Align.DECIMAL && offset != null) {
                padded = padDecimal(content, target, offset);
            } else {
                padded = pad(content, target, align);
            }

            sb.append(styleStr)
                    .append(padded)
                    .append(RESET)
                    .append("|");
            colIdx += span;
        }
        sb.append("\n");
        return sb.toString();
    }
}