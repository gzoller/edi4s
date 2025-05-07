package co.blocke.edi4j.model4j.difference;

import co.blocke.edi4j.model4j.Path;
import java.util.*;
import java.util.AbstractMap.SimpleEntry;

public class HLSegmentDifference implements SegmentDifference {
    private final Path path;
    private final String name;
    private final String canonicalName;
    private final SimpleEntry<Boolean, Boolean> presence;
    private final SimpleEntry<Boolean, Boolean> required;

    public HLSegmentDifference(
            Path path,
            String name,
            String canonicalName,
            SimpleEntry<Boolean, Boolean> presence,
            SimpleEntry<Boolean, Boolean> required
    ) {
        this.path = path;
        this.name = name;
        this.canonicalName = canonicalName;
        this.presence = presence;
        this.required = required;
    }

    @Override public Path getPath()            { return path; }
    @Override public String getName()          { return name; }
    @Override public String getCanonicalName(){ return canonicalName; }
    @Override public SimpleEntry<Boolean, Boolean> getPresence()  { return presence; }
    @Override public SimpleEntry<Boolean, Boolean> getRequired()  { return required; }
    @Override public Optional<SimpleEntry<List<String>, List<String>>> getAssertions() { return Optional.empty(); }
    @Override public List<FieldDifference> getFieldDiff() { return Collections.emptyList(); }
}
