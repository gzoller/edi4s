package co.blocke.edi4j.model4j.difference;

import co.blocke.edi4j.model4j.Path;
import co.blocke.edi4j.table.*;
import java.util.*;
import java.util.AbstractMap.SimpleEntry;

public class SimpleSegmentDifference implements SegmentDifference {
    private final Path path;
    private final String name;
    private final String canonicalName;
    private final SimpleEntry<Boolean, Boolean> presence;
    private final SimpleEntry<Boolean, Boolean> required;
    private final Optional<SimpleEntry<List<String>, List<String>>> assertions;
    private final List<FieldDifference> fieldDiff;

    public SimpleSegmentDifference(
            Path path,
            String name,
            String canonicalName,
            SimpleEntry<Boolean, Boolean> presence,
            SimpleEntry<Boolean, Boolean> required,
            Optional<SimpleEntry<List<String>, List<String>>> assertions,
            List<FieldDifference> fieldDiff
    ) {
        this.path = path;
        this.name = name;
        this.canonicalName = canonicalName;
        this.presence = presence;
        this.required = required;
        this.assertions = assertions;
        this.fieldDiff = fieldDiff;
    }

    @Override public Path getPath()               { return path; }
    @Override public String getName()             { return name; }
    @Override public String getCanonicalName()    { return canonicalName; }
    @Override public SimpleEntry<Boolean, Boolean> getPresence()  { return presence; }
    @Override public SimpleEntry<Boolean, Boolean> getRequired()  { return required; }
    @Override public Optional<SimpleEntry<List<String>, List<String>>> getAssertions() { return assertions; }
    @Override public List<FieldDifference> getFieldDiff() { return fieldDiff; }
}