/*
 * This file was automatically generated by EvoSuite
 * Sat May 16 12:52:16 UTC 2020
 */

package org.xwiki.model.internal.reference;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.evosuite.runtime.javaee.injection.Injector;
import org.junit.runner.RunWith;
import org.xwiki.model.EntityType;
import org.xwiki.model.internal.reference.AbstractStringEntityReferenceResolver;
import org.xwiki.model.internal.reference.DefaultSymbolScheme;
import org.xwiki.model.internal.reference.ExplicitStringEntityReferenceResolver;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class AbstractStringEntityReferenceResolver_ESTest extends AbstractStringEntityReferenceResolver_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      Character character0 = new Character('+');
      DefaultSymbolScheme defaultSymbolScheme0 = new DefaultSymbolScheme();
      defaultSymbolScheme0.getEscapeSymbol();
      defaultSymbolScheme0.initialize();
      Character character1 = new Character('{');
      Character.getName(4);
      EntityType entityType0 = EntityType.SPACE;
      defaultSymbolScheme0.initialize();
      Object[] objectArray0 = new Object[9];
      objectArray0[0] = (Object) null;
      defaultSymbolScheme0.initialize();
      defaultSymbolScheme0.initialize();
      Character.compare('+', '+');
      objectArray0[1] = (Object) null;
      objectArray0[2] = (Object) " with 1 type parameter: class expects ";
      objectArray0[5] = (Object) character1;
      objectArray0[6] = (Object) defaultSymbolScheme0;
      Object object0 = new Object();
      objectArray0[7] = object0;
      objectArray0[8] = (Object) entityType0;
      ExplicitStringEntityReferenceResolver explicitStringEntityReferenceResolver0 = new ExplicitStringEntityReferenceResolver();
      DefaultSymbolScheme defaultSymbolScheme1 = new DefaultSymbolScheme();
      Injector.inject(explicitStringEntityReferenceResolver0, (Class<?>) AbstractStringEntityReferenceResolver.class, "symbolScheme", (Object) defaultSymbolScheme1);
      Injector.validateBean(explicitStringEntityReferenceResolver0, (Class<?>) ExplicitStringEntityReferenceResolver.class);
      explicitStringEntityReferenceResolver0.initialize();
      explicitStringEntityReferenceResolver0.initialize();
      explicitStringEntityReferenceResolver0.getSymbolScheme();
      explicitStringEntityReferenceResolver0.initialize();
      EntityType entityType1 = EntityType.ATTACHMENT;
      // Undeclared exception!
      explicitStringEntityReferenceResolver0.resolve("END OF TRANSMISSION", entityType1, objectArray0);
  }
}
