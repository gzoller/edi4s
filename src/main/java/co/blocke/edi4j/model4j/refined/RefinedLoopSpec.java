// RefinedLoopSpec.java
package co.blocke.edi4j.model4j.refined;

import java.util.List;
import java.util.Optional;
import com.fasterxml.jackson.annotation.JsonIgnore;

public class RefinedLoopSpec implements RefinedSingleOrLoopSegmentSpec {
    private final String name;
    private final String canonicalName;
    private final Optional<String> humanName;
    private final String description;
    private final boolean required;
    private final List<String> assertions;
    private final List<RefinedFieldSpec> fields;
    private final Optional<Integer> minRepeats;
    private final Optional<Integer> maxRepeats;
    private final List<RefinedSingleOrLoopSegmentSpec> body;
    private final Optional<RefinedLoopSpec> nested;

    public RefinedLoopSpec(
        String name,
        String canonicalName,
        Optional<String> humanName,
        String description,
        boolean required,
        List<String> assertions,
        List<RefinedFieldSpec> fields,
        Optional<Integer> minRepeats,
        Optional<Integer> maxRepeats,
        List<RefinedSingleOrLoopSegmentSpec> body,
        Optional<RefinedLoopSpec> nested
    ) {
        this.name           = name;
        this.canonicalName  = canonicalName;
        this.humanName      = humanName;
        this.description    = description;
        this.required       = required;
        this.assertions     = assertions;
        this.fields         = fields;
        this.minRepeats     = minRepeats;
        this.maxRepeats     = maxRepeats;
        this.body           = body;
        this.nested         = nested;
    }

    @Override public String getName()          { return name; }
    @Override public String getCanonicalName() { return canonicalName; }
    @Override public Optional<String> getHumanName() { return humanName; }
    @Override public String getDescription()   { return description; }
    @Override public boolean isRequired()      { return required; }
    @Override public List<String> getAssertions() { return assertions; }
    @Override public List<RefinedFieldSpec> getFields() { return fields; }

    /** EDI‐style “struct” loops (min=1, max=1) */
    @JsonIgnore
    public boolean isStruct() {
        return minRepeats.isPresent()
            && maxRepeats.isPresent()
            && minRepeats.get() == 1
            && maxRepeats.get() == 1;
    }

    /** true if maxRepeats > 1 */
    @JsonIgnore
    public boolean isRepeatable() {
        return maxRepeats.isPresent()
            && maxRepeats.get() > 1;
    }

    public Optional<Integer> getMinRepeats()  { return minRepeats; }
    public Optional<Integer> getMaxRepeats()  { return maxRepeats; }
    public List<RefinedSingleOrLoopSegmentSpec> getBody() { return body; }
    public Optional<RefinedLoopSpec> getNested() { return nested; }
}

