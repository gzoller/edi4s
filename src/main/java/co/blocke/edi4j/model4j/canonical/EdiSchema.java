// EdiSchema.java
package co.blocke.edi4j.model4j.canonical;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Optional;
import com.fasterxml.jackson.annotation.JsonProperty;

public class EdiSchema implements EnumOrSchema {
    private final Optional<List<String>> required;
    private final String type;
    private final LinkedHashMap<String, Property> properties;
    private final Optional<String> xOpenediSegmentId;
    private final Optional<String> xOpenediCompositeId;
    private final Optional<String> xOpenediLoopId;
    private final Optional<List<String>> xOpenediSyntax;

    public EdiSchema(
        Optional<List<String>> required,
        String type,
        LinkedHashMap<String, Property> properties,
        @JsonProperty("x-openedi-segment-id") Optional<String> xOpenediSegmentId,
        @JsonProperty("x-openedi-composite-id") Optional<String> xOpenediCompositeId,
        @JsonProperty("x-openedi-loop-id") Optional<String> xOpenediLoopId,
        @JsonProperty("x-openedi-syntax") Optional<List<String>> xOpenediSyntax
    ) {
        this.required            = required;
        this.type                = type;
        this.properties          = properties;
        this.xOpenediSegmentId   = xOpenediSegmentId;
        this.xOpenediCompositeId = xOpenediCompositeId;
        this.xOpenediLoopId      = xOpenediLoopId;
        this.xOpenediSyntax      = xOpenediSyntax;
    }

    public boolean isRequired(String p) {
        return required.map(list -> list.contains(p)).orElse(false);
    }

    public boolean isComposite() {
        return xOpenediCompositeId.isPresent();
    }

    public boolean isLoop() {
        return xOpenediLoopId.isPresent();
    }

    public String getId() {
        return xOpenediSegmentId
               .or(() -> xOpenediCompositeId)
               .or(() -> xOpenediLoopId)
               .orElse("unknown");
    }

    public LinkedHashMap<String, Property> getProperties() {
        return properties;
    }

    public Optional<List<String>> getRequired() {
        return required;
    }

    public String getType() {
        return type;
    }

    public Optional<String> getxOpenediSegmentId() {
        return xOpenediSegmentId;
    }

    public Optional<String> getxOpenediCompositeId() {
        return xOpenediCompositeId;
    }

    public Optional<String> getxOpenediLoopId() {
        return xOpenediLoopId;
    }

    public Optional<List<String>> getxOpenediSyntax() {
        return xOpenediSyntax;
    }
}

