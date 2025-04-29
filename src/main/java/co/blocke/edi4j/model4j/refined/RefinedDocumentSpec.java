// RefinedDocumentSpec.java
package co.blocke.edi4j.model4j.refined;

import java.util.List;

public class RefinedDocumentSpec {
    private final String name;
    private final String version;
    private final String partner;
    private final List<RefinedSingleOrLoopSegmentSpec> segments;

    public RefinedDocumentSpec(
        String name,
        String version,
        String partner,
        List<RefinedSingleOrLoopSegmentSpec> segments
    ) {
        this.name     = name;
        this.version  = version;
        this.partner  = partner;
        this.segments = segments;
    }

    public String getName()       { return name; }
    public String getVersion()    { return version; }
    public String getPartner()    { return partner; }
    public List<RefinedSingleOrLoopSegmentSpec> getSegments() { return segments; }
}

