package co.blocke.edi4j.model4j.difference;

import co.blocke.edi4j.table.*;
import java.util.*;
import java.util.AbstractMap.SimpleEntry;

public final class DifferenceModels {
    private DifferenceModels() {}

    public static boolean isMuted(Row row) {
        return row.getCells().stream()
                .allMatch(c -> c.getStyle().filter(s -> s == Style.MUTED).isPresent());
    }

    public static Row presenceRow(
            String label,
            int nestLevel,
            SimpleEntry<Boolean, Boolean> presence,
            SimpleEntry<Boolean, Boolean> required,
            Style okStyle
    ) {
        boolean p1 = presence.getKey();
        boolean p2 = presence.getValue();
        boolean r2 = required.getValue();

        if (p2 == false) {
            return new Row(Arrays.asList(
                    new Cell(label, Optional.of(Style.MUTED), Optional.empty(), nestLevel, 1),
                    new Cell("skipped", Optional.of(Style.MUTED)),
                    new Cell(label, Optional.of(Style.MUTED), Optional.empty(), nestLevel, 1),
                    new Cell("missing", Optional.of(Style.MUTED))
            ));
        } else if (!p1 && !r2) {
            return new Row(Arrays.asList(
                    new Cell(label, Optional.of(Style.MUTED), Optional.empty(), nestLevel, 1),
                    new Cell("missing", Optional.of(Style.MUTED)),
                    new Cell(label, Optional.of(Style.MUTED), Optional.empty(), nestLevel, 1),
                    new Cell("optional", Optional.of(Style.MUTED))
            ));
        } else if (!p1 && r2) {
            return new Row(Arrays.asList(
                    new Cell(label, Optional.of(Style.MUTED), Optional.empty(), nestLevel, 1),
                    new Cell("missing", Optional.of(Style.WARN)),
                    new Cell(label, Optional.of(okStyle), Optional.empty(), nestLevel, 1),
                    new Cell("required", Optional.of(Style.WARN))
            ));
        } else if (p1 && required.getKey() == false && required.getValue()) {
            return new Row(Arrays.asList(
                    new Cell(label, Optional.of(okStyle), Optional.empty(), nestLevel, 1),
                    new Cell("optional", Optional.of(Style.WARN)),
                    new Cell(label, Optional.of(okStyle), Optional.empty(), nestLevel, 1),
                    new Cell("required", Optional.of(Style.WARN))
            ));
        } else if (p1 && required.getKey() && !required.getValue()) {
            return new Row(Arrays.asList(
                    new Cell(label, Optional.of(okStyle), Optional.empty(), nestLevel, 1),
                    new Cell("OK (required)", Optional.of(Style.TERTIARY)),
                    new Cell(label, Optional.of(okStyle), Optional.empty(), nestLevel, 1),
                    new Cell("OK (optional)", Optional.of(Style.TERTIARY))
            ));
        } else {
            return new Row(Arrays.asList(
                    new Cell(label, Optional.of(okStyle), Optional.empty(), nestLevel, 1),
                    new Cell("OK", Optional.of(Style.TERTIARY)),
                    new Cell(label, Optional.of(okStyle), Optional.empty(), nestLevel, 1),
                    new Cell("OK", Optional.of(Style.TERTIARY))
            ));
        }
    }
}