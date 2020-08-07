/*
 * This file was automatically generated by EvoSuite
 * Sun May 17 17:46:55 UTC 2020
 */

package org.xwiki.model.reference;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.shaded.org.mockito.Mockito.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.evosuite.runtime.ViolatedAssumptionAnswer;
import org.junit.runner.RunWith;
import org.xwiki.model.reference.AttachmentReference;
import org.xwiki.model.reference.DocumentReference;
import org.xwiki.model.reference.EntityReference;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class AttachmentReference_ESTest extends AttachmentReference_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      DocumentReference documentReference0 = mock(DocumentReference.class, new ViolatedAssumptionAnswer());
      AttachmentReference attachmentReference0 = new AttachmentReference("PyJ\"H9[}vIk@FSJ", documentReference0);
      DocumentReference documentReference1 = mock(DocumentReference.class, new ViolatedAssumptionAnswer());
      doReturn((String) null).when(documentReference1).toString();
      AttachmentReference attachmentReference1 = new AttachmentReference("PyJ\"H9[}vIk@FSJ", documentReference1);
      EntityReference entityReference0 = attachmentReference1.getParent();
      attachmentReference0.equalsNonRecursive(entityReference0);
      attachmentReference0.getParameter("PyJ\"H9[}vIk@FSJ");
      attachmentReference0.getType();
      DocumentReference documentReference2 = mock(DocumentReference.class, new ViolatedAssumptionAnswer());
      AttachmentReference attachmentReference2 = new AttachmentReference("", documentReference2);
  }
}
