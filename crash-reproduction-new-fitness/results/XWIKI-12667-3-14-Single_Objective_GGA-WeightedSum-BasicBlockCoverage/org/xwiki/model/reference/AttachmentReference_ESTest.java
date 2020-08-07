/*
 * This file was automatically generated by EvoSuite
 * Thu May 14 13:26:46 UTC 2020
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
import org.xwiki.model.EntityType;
import org.xwiki.model.reference.AttachmentReference;
import org.xwiki.model.reference.DocumentReference;
import org.xwiki.model.reference.EntityReference;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class AttachmentReference_ESTest extends AttachmentReference_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      DocumentReference documentReference0 = mock(DocumentReference.class, new ViolatedAssumptionAnswer());
      doReturn((EntityReference) null).when(documentReference0).getParent();
      AttachmentReference attachmentReference0 = new AttachmentReference("Invalid type [", documentReference0);
      String string0 = "N24Zg9r@>pcw2\",";
      DocumentReference documentReference1 = mock(DocumentReference.class, new ViolatedAssumptionAnswer());
      doReturn((EntityReference) null).when(documentReference1).getParent();
      doReturn((EntityType) null).when(documentReference1).getType();
      AttachmentReference attachmentReference1 = new AttachmentReference("N24Zg9r@>pcw2\",", documentReference1);
      attachmentReference0.getRoot();
      attachmentReference1.getDocumentReference();
      String string1 = "";
      AttachmentReference attachmentReference2 = new AttachmentReference("", (DocumentReference) null);
  }
}
