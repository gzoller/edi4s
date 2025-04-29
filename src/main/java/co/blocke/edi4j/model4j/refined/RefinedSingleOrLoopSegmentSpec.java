// RefinedSingleOrLoopSegmentSpec.java
package co.blocke.edi4j.model4j.refined;

import java.util.List;
import java.util.Optional;

public interface RefinedSingleOrLoopSegmentSpec {
    String getName();
    String getCanonicalName();
    Optional<String> getHumanName();
    String getDescription();
    boolean isRequired();
    List<String> getAssertions();
    List<RefinedFieldSpec> getFields();
}

