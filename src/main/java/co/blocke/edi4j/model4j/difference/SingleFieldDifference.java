package co.blocke.edi4j.model4j.difference;

import co.blocke.edi4j.model4j.Path;
import co.blocke.edi4j.table.*;
import java.util.*;
import java.util.AbstractMap.SimpleEntry;

public class SingleFieldDifference implements FieldDifference {
    private final Path path;
    private final String name;
    private final String canonicalName;
    private final SimpleEntry<Boolean, Boolean> presence;
    private final SimpleEntry<Boolean, Boolean> required;
    private final Optional<SimpleEntry<String, String>> dataType;
    private final Optional<SimpleEntry<Optional<String>, Optional<String>>> format;
    private final Optional<SimpleEntry<Optional<Integer>, Optional<Integer>>> elementId;
    private final Optional<SimpleEntry<List<String>, List<String>>> validValues;
    private final Optional<SimpleEntry<Optional<String>, Optional<String>>> validValuesRef;

    public SingleFieldDifference(
            Path path,
            String name,
            String canonicalName,
            SimpleEntry<Boolean, Boolean> presence,
            SimpleEntry<Boolean, Boolean> required,
            Optional<SimpleEntry<String, String>> dataType,
            Optional<SimpleEntry<Optional<String>, Optional<String>>> format,
            Optional<SimpleEntry<Optional<Integer>, Optional<Integer>>> elementId,
            Optional<SimpleEntry<List<String>, List<String>>> validValues,
            Optional<SimpleEntry<Optional<String>, Optional<String>>> validValuesRef
    ) {
        this.path = path;
        this.name = name;
        this.canonicalName = canonicalName;
        this.presence = presence;
        this.required = required;
        this.dataType = dataType;
        this.format = format;
        this.elementId = elementId;
        this.validValues = validValues;
        this.validValuesRef = validValuesRef;
    }

    @Override public Path getPath()            { return path; }
    @Override public String getName()          { return name; }
    @Override public String getCanonicalName(){ return canonicalName; }
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

        dataType.ifPresent(pair -> {
            rows.add(new Row(
                    Arrays.asList(
                            new Cell("--Data Type:", Optional.of(Style.TERTIARY)),
                            new Cell(pair.getKey(), Optional.of(Style.NEUTRAL)),
                            new Cell("--Data Type:", Optional.of(Style.TERTIARY)),
                            new Cell(pair.getValue(), Optional.of(Style.NEUTRAL))
                    )
            ));
        });

        format.ifPresent(pair -> {
            Optional<String> a = pair.getKey();
            Optional<String> b = pair.getValue();
            Optional<Style> labelStyle;
            Optional<Style> matchStyle;
            if (!a.isPresent() && b.isPresent()) {
                labelStyle = Optional.of(Style.TERTIARY);
                matchStyle = Optional.of(Style.WARN);
            } else if (!b.isPresent()) {
                labelStyle = Optional.of(Style.MUTED);
                matchStyle = Optional.of(Style.MUTED);
            } else {
                labelStyle = Optional.of(Style.TERTIARY);
                matchStyle = Optional.of(Style.WARN);
            }
            rows.add(new Row(
                    Arrays.asList(
                            new Cell("--Format:", labelStyle),
                            new Cell(a.orElse("(not given)"), matchStyle),
                            new Cell("--Format:", labelStyle),
                            new Cell(b.orElse("(not given)"), matchStyle)
                    )
            ));
        });

        elementId.ifPresent(pair -> {
            Optional<Integer> a = pair.getKey();
            Optional<Integer> b = pair.getValue();
            Optional<Style> labelStyle;
            Optional<Style> matchStyle;
            if (!a.isPresent() && b.isPresent()) {
                labelStyle = Optional.of(Style.TERTIARY);
                matchStyle = Optional.of(Style.WARN);
            } else if (!b.isPresent()) {
                labelStyle = Optional.of(Style.MUTED);
                matchStyle = Optional.of(Style.MUTED);
            } else {
                labelStyle = Optional.of(Style.TERTIARY);
                matchStyle = Optional.of(Style.WARN);
            }
            rows.add(new Row(
                    Arrays.asList(
                            new Cell("--Element ID:", labelStyle),
                            new Cell(a.map(Object::toString).orElse("(not given)"), matchStyle),
                            new Cell("--Element ID:", labelStyle),
                            new Cell(b.map(Object::toString).orElse("(not given)"), matchStyle)
                    )
            ));
        });

        validValues.ifPresent(pair -> {
            List<String> a = pair.getKey();
            List<String> b = pair.getValue();
            String matchValueA;
            String matchValueB;
            Optional<Style> vsA;
            Optional<Style> vsB;
            if (a.stream().allMatch(b::contains)) {
                matchValueA = "OK";
                matchValueB = "OK";
                vsA = Optional.of(Style.TERTIARY);
                vsB = Optional.of(Style.TERTIARY);
            } else {
                matchValueA = String.join(",", a);
                matchValueB = String.join(",", b);
                vsA = Optional.of(Style.WARN);
                vsB = Optional.of(Style.WARN);
            }
            rows.add(new Row(
                    Arrays.asList(
                            new Cell("--Valid Values:", Optional.of(Style.TERTIARY)),
                            new Cell(matchValueA, vsA),
                            new Cell("--Valid Values:", Optional.of(Style.TERTIARY)),
                            new Cell(matchValueB, vsB)
                    )
            ));
        });

        validValuesRef.ifPresent(pair -> {
            Optional<String> a = pair.getKey();
            Optional<String> b = pair.getValue();
            rows.add(new Row(
                    Arrays.asList(
                            new Cell("--Valid Values Ref:", Optional.of(Style.TERTIARY)),
                            new Cell(a.orElse("(not given)"), Optional.of(Style.WARN)),
                            new Cell("--Valid Values Ref:", Optional.of(Style.TERTIARY)),
                            new Cell(b.orElse("(not given)"), Optional.of(Style.WARN))
                    )
            ));
        });

        return rows;
    }
}