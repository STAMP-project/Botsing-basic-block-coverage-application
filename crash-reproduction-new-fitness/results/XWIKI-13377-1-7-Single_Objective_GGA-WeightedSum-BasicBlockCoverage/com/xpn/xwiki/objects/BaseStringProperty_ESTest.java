/*
 * This file was automatically generated by EvoSuite
 * Thu May 14 13:57:51 UTC 2020
 */

package com.xpn.xwiki.objects;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.shaded.org.mockito.Mockito.*;
import static org.evosuite.runtime.EvoAssertions.*;
import com.xpn.xwiki.objects.BaseStringProperty;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.evosuite.runtime.ViolatedAssumptionAnswer;
import org.junit.runner.RunWith;
import org.xwiki.model.reference.DocumentReference;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class BaseStringProperty_ESTest extends BaseStringProperty_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      BaseStringProperty baseStringProperty0 = new BaseStringProperty();
      BaseStringProperty baseStringProperty1 = baseStringProperty0.clone();
      baseStringProperty0.equals(baseStringProperty1);
      DocumentReference documentReference0 = mock(DocumentReference.class, new ViolatedAssumptionAnswer());
      baseStringProperty0.setDocumentReference(documentReference0);
      // Undeclared exception!
      baseStringProperty1.setValue(baseStringProperty0);
  }
}
