package co.blocke.edi4j.model4j.difference;

import co.blocke.edi4j.model4j.Path;
import co.blocke.edi4j.table.*;
import java.util.*;
import java.util.AbstractMap.SimpleEntry;

public class LoopSegmentDifference implements SegmentDifference {
    private final Path path;
    private final String name;
    private final String canonicalName;
    private final SimpleEntry<Boolean, Boolean> presence;
    private final SimpleEntry<Boolean, Boolean> required;
    private final Optional<SimpleEntry<List<String>, List<String>>> assertions;
    private final List<FieldDifference> fieldDiff;
    private final Optional<SimpleEntry<Optional<Integer>, Optional<Integer>>> minDiff;
    private final Optional<SimpleEntry<Optional<Integer>, Optional<Integer>>> maxDiff;
    private final List<SegmentDifference> bodyDiff;
    private final Optional<List<LoopSegmentDifference>> nested;

    public LoopSegmentDifference(
            Path path,
            String name,
            String canonicalName,
            SimpleEntry<Boolean, Boolean> presence,
            SimpleEntry<Boolean, Boolean> required,
            Optional<SimpleEntry<List<String>, List<String>>> assertions,
            List<FieldDifference> fieldDiff,
            Optional<SimpleEntry<Optional<Integer>, Optional<Integer>>> minDiff,
            Optional<SimpleEntry<Optional<Integer>, Optional<Integer>>> maxDiff,
            List<SegmentDifference> bodyDiff,
            Optional<List<LoopSegmentDifference>> nested
    ) {
        this.path = path;
        this.name = name;
        this.canonicalName = canonicalName;
        this.presence = presence;
        this.required = required;
        this.assertions = assertions;
        this.fieldDiff = fieldDiff;
        this.minDiff = minDiff;
        this.maxDiff = maxDiff;
        this.bodyDiff = bodyDiff;
        this.nested = nested;
    }

    @Override public Path getPath()            { return path; }
    @Override public String getName()          { return name; }
    @Override public String getCanonicalName(){ return canonicalName; }
    @Override public SimpleEntry<Boolean, Boolean> getPresence()  { return presence; }
    @Override public SimpleEntry<Boolean, Boolean> getRequired()  { return required; }
    @Override public Optional<SimpleEntry<List<String>, List<String>>> getAssertions() { return assertions; }
    @Override public List<FieldDifference> getFieldDiff() { return fieldDiff; }

    @Override
    public List<BodyRow> render(int nestLevel) {
        List<BodyRow> base = SegmentDifference.super.render(nestLevel);
        List<BodyRow> rows = new ArrayList<>(base);

        minDiff.ifPresent(pair -> {
            Optional<Integer> a = pair.getKey();
            Optional<Integer> b = pair.getValue();
            Optional<Style> matchStyle = (!a.isPresent() && b.isPresent())
                    ? Optional.of(Style.WARN)
                    : Optional.of(Style.NEUTRAL);
            rows.add(new Row(Arrays.asList(
                    new Cell("--Min Repeats:", Optional.of(Style.TERTIARY)),
                    new Cell(a.map(Object::toString).orElse("(not given)"), matchStyle),
                    new Cell("--Min Repeats:", Optional.of(Style.TERTIARY)),
                    new Cell(b.map(Object::toString).orElse("(not given)"), matchStyle)
            )));
        });

        maxDiff.ifPresent(pair -> {
            Optional<Integer> a = pair.getKey();
            Optional<Integer> b = pair.getValue();
            Optional<Style> matchStyle = (!a.isPresent() && b.isPresent())
                    ? Optional.of(Style.WARN)
                    : Optional.of(Style.NEUTRAL);
            rows.add(new Row(Arrays.asList(
                    new Cell("--Max Repeats:", Optional.of(Style.TERTIARY)),
                    new Cell(a.map(Object::toString).orElse("(not given)"), matchStyle),
                    new Cell("--Max Repeats:", Optional.of(Style.TERTIARY)),
                    new Cell(b.map(Object::toString).orElse("(not given)"), matchStyle)
            )));
        });

        bodyDiff.forEach(sd -> rows.addAll(sd.render(nestLevel + 1)));

        nested.ifPresent(list -> list.forEach(n -> {
            SimpleEntry<Boolean, Boolean> pres = n.getPresence();
            boolean req1 = n.getRequired().getKey();
            boolean req2 = n.getRequired().getValue();
            if (pres.getKey() && !pres.getValue()) {
                rows.add(new Row(Arrays.asList(
                        new Cell(n.getCanonicalName(), Optional.of(Style.ALERT)),
                        new Cell(req1 ? "required" : "optional", Optional.of(Style.ALERT)),
                        new Cell(n.getCanonicalName(), Optional.of(Style.ALERT)),
                        new Cell("missing", Optional.of(Style.ALERT))
                )));
            } else if (!pres.getKey() && pres.getValue() && !req2) {
                // no-op
            } else if (!pres.getKey() && pres.getValue()) {
                rows.add(new Row(Arrays.asList(
                        new Cell(n.getCanonicalName(), Optional.of(Style.ALERT)),
                        new Cell("missing", Optional.of(Style.ALERT)),
                        new Cell(n.getCanonicalName(), Optional.of(Style.ALERT)),
                        new Cell(req1 ? "required" : "optional", Optional.of(Style.ALERT))
                )));
            } else {
                rows.addAll(n.render(nestLevel + 1));
            }
        }));

        return rows;
    }
}
