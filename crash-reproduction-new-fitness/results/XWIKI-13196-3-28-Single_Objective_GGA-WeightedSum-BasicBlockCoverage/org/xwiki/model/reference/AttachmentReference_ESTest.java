/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 00:48:23 UTC 2020
 */

package org.xwiki.model.reference;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.shaded.org.mockito.Mockito.*;
import static org.evosuite.runtime.EvoAssertions.*;
import java.io.Serializable;
import java.util.Map;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.evosuite.runtime.ViolatedAssumptionAnswer;
import org.junit.runner.RunWith;
import org.xwiki.model.reference.AttachmentReference;
import org.xwiki.model.reference.DocumentReference;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class AttachmentReference_ESTest extends AttachmentReference_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      DocumentReference documentReference0 = mock(DocumentReference.class, new ViolatedAssumptionAnswer());
      AttachmentReference attachmentReference0 = new AttachmentReference("*p>|R^Vzua)%hUF-c", documentReference0);
      Map<String, Serializable> map0 = attachmentReference0.getParameters();
      attachmentReference0.setParameters(map0);
      String string0 = "ue ]8{3Crf!p*7";
      DocumentReference documentReference1 = null;
      AttachmentReference attachmentReference1 = new AttachmentReference((String) null, (DocumentReference) null);
  }
}
