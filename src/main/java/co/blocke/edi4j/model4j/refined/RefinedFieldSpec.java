// RefinedFieldSpec.java
package co.blocke.edi4j.model4j.refined;

import java.util.Optional;

public interface RefinedFieldSpec {
    String getName();
    String getCanonicalName();
    Optional<String> getHumanName();
    int getIndex();
    String getDescription();
    boolean isRequired();
}

