package scala.meta.pc;


public interface ReferenceCountProvider {

    Integer references(String buildTargetIdentifier, String symbol);
}
