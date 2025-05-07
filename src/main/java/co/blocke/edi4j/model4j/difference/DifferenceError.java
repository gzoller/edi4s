package co.blocke.edi4j.model4j.difference;

import co.blocke.edi4j.model4j.Path;
import co.blocke.edi4j.table.*;
import java.util.*;
import java.util.AbstractMap.SimpleEntry;

public class DifferenceError implements SegmentDifference {
    private final Path path;
    private final String message;

    public DifferenceError(Path path, String message) {
        this.path = path;
        this.message = message;
    }

    @Override public Path getPath()            { return path; }
    @Override public String getName()          { return ""; }
    @Override public String getCanonicalName(){ return ""; }
    @Override public SimpleEntry<Boolean, Boolean> getPresence()  { return new SimpleEntry<>(true, true); }
    @Override public SimpleEntry<Boolean, Boolean> getRequired()  { return new SimpleEntry<>(true, true); }
    @Override public Optional<SimpleEntry<List<String>, List<String>>> getAssertions() { return Optional.empty(); }
    @Override public List<FieldDifference> getFieldDiff() { return Collections.emptyList(); }

    @Override public List<BodyRow> render(int nestLevel) { return Collections.emptyList(); }
}
