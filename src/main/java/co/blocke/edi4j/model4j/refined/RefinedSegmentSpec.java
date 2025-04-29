// RefinedSegmentSpec.java
package co.blocke.edi4j.model4j.refined;

import java.util.List;
import java.util.Optional;

public class RefinedSegmentSpec implements RefinedSingleOrLoopSegmentSpec {
    private final String name;
    private final String canonicalName;
    private final Optional<String> humanName;
    private final String description;
    private final boolean required;
    private final List<String> assertions;
    private final List<RefinedFieldSpec> fields;

    public RefinedSegmentSpec(
        String name,
        String canonicalName,
        Optional<String> humanName,
        String description,
        boolean required,
        List<String> assertions,
        List<RefinedFieldSpec> fields
    ) {
        this.name          = name;
        this.canonicalName = canonicalName;
        this.humanName     = humanName;
        this.description   = description;
        this.required      = required;
        this.assertions    = assertions;
        this.fields        = fields;
    }

    @Override public String getName()           { return name; }
    @Override public String getCanonicalName()  { return canonicalName; }
    @Override public Optional<String> getHumanName() { return humanName; }
    @Override public String getDescription()    { return description; }
    @Override public boolean isRequired()       { return required; }
    @Override public List<String> getAssertions() { return assertions; }
    @Override public List<RefinedFieldSpec> getFields() { return fields; }
}

