package co.blocke.edi4j.model4j.difference;

import co.blocke.edi4j.model4j.Path;
import co.blocke.edi4j.table.*;
import java.util.*;
import java.util.AbstractMap.SimpleEntry;

public class CompositeFieldDifference implements FieldDifference {
    private final Path path;
    private final String name;
    private final String canonicalName;
    private final SimpleEntry<Boolean, Boolean> presence;
    private final SimpleEntry<Boolean, Boolean> required;
    private final List<FieldDifference> fieldDiff;

    public CompositeFieldDifference(
            Path path,
            String name,
            String canonicalName,
            SimpleEntry<Boolean, Boolean> presence,
            SimpleEntry<Boolean, Boolean> required,
            List<FieldDifference> fieldDiff
    ) {
        this.path = path;
        this.name = name;
        this.canonicalName = canonicalName;
        this.presence = presence;
        this.required = required;
        this.fieldDiff = fieldDiff;
    }

    @Override public Path getPath()             { return path; }
    @Override public String getName()           { return name; }
    @Override public String getCanonicalName()  { return canonicalName; }
    @Override public SimpleEntry<Boolean, Boolean> getPresence() { return presence; }
    @Override public SimpleEntry<Boolean, Boolean> getRequired() { return required; }

    @Override
    public List<BodyRow> render(int nestLevel) {
        String rationalName = name.equals(canonicalName)
                ? name
                : canonicalName + " (" + name + ")";
        Row presenceRow = DifferenceModels.presenceRow(
                rationalName,
                nestLevel,
                presence,
                required,
                Style.NEUTRAL
        );
        List<BodyRow> rows = new ArrayList<>();
        rows.add(presenceRow);
        for (FieldDifference fd : fieldDiff) {
            rows.addAll(fd.render(nestLevel + 1));
        }
        return rows;
    }
}