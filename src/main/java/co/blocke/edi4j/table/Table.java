package co.blocke.edi4j.table;

import java.util.*;

public class Table {
    public static final Map<String, Map<Style, String>> DEFAULT_STYLE_MAP;
    static {
        Map<Style, String> textMap = new EnumMap<>(Style.class);
        textMap.put(Style.PRIMARY,   "\u001b[1m\u001b[34m");
        textMap.put(Style.SECONDARY, "\u001b[1m\u001b[36m");
        textMap.put(Style.TERTIARY,  "\u001b[32m");
        textMap.put(Style.NEUTRAL,   "");
        textMap.put(Style.MUTED,     "\u001b[2m");
        textMap.put(Style.ALERT,     "\u001b[31m");
        textMap.put(Style.WARN,      "\u001b[38;5;180m");
        Map<String, Map<Style, String>> sm = new HashMap<>();
        sm.put("text", Collections.unmodifiableMap(textMap));
        DEFAULT_STYLE_MAP = Collections.unmodifiableMap(sm);
    }

    private final List<Title> title;
    private final int columns;
    private final List<Integer> columnWidthPct;
    private final int tableWidth;
    private final Header header;
    private final List<BodyRow> rows;
    private final Map<String, Map<Style, String>> styleMap;

    public Table(List<Title> title,
                 int columns,
                 List<Integer> columnWidthPct,
                 int tableWidth,
                 Header header,
                 List<BodyRow> rows) {
        this(title, columns, columnWidthPct, tableWidth, header, rows, DEFAULT_STYLE_MAP);
    }

    public Table(List<Title> title,
                 int columns,
                 List<Integer> columnWidthPct,
                 int tableWidth,
                 Header header,
                 List<BodyRow> rows,
                 Map<String, Map<Style, String>> styleMap) {
        this.title = Objects.requireNonNull(title);
        this.columns = columns;
        this.columnWidthPct = Objects.requireNonNull(columnWidthPct);
        this.tableWidth = tableWidth;
        this.header = Objects.requireNonNull(header);
        this.rows = Objects.requireNonNull(rows);
        this.styleMap = Objects.requireNonNull(styleMap);
    }

    @Override
    public String toString() {
        return new TextRenderer(this).render();
    }

    public String toHtml() {
        return new HtmlRenderer(this).render();
    }

    public List<Title> getTitle() {
        return title;
    }

    public int getColumns() {
        return columns;
    }

    public List<Integer> getColumnWidthPct() {
        return columnWidthPct;
    }

    public int getTableWidth() {
        return tableWidth;
    }

    public Header getHeader() {
        return header;
    }

    public List<BodyRow> getRows() {
        return rows;
    }

    public Map<String, Map<Style, String>> getStyleMap() {
        return styleMap;
    }

    // (Optional) static accessor for the default map:
    public static Map<String, Map<Style, String>> getDefaultStyleMap() {
        return DEFAULT_STYLE_MAP;
    }
}