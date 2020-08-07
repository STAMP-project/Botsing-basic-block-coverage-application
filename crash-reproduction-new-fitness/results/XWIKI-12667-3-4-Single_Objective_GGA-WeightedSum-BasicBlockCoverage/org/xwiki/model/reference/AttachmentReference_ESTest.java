/*
 * This file was automatically generated by EvoSuite
 * Thu May 14 13:18:21 UTC 2020
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
      AttachmentReference attachmentReference0 = new AttachmentReference("Provided reference should not be null", documentReference0);
      EntityType entityType0 = EntityType.WIKI;
      attachmentReference0.equals((EntityReference) attachmentReference0, entityType0, entityType0);
      DocumentReference documentReference1 = mock(DocumentReference.class, new ViolatedAssumptionAnswer());
      AttachmentReference attachmentReference1 = new AttachmentReference("$U`j&KxpLT,?-!|j", documentReference1);
      attachmentReference1.getParameter("");
      attachmentReference0.setName("hhd&$#I/GH~9");
      attachmentReference0.getType();
      DocumentReference documentReference2 = mock(DocumentReference.class, new ViolatedAssumptionAnswer());
      AttachmentReference attachmentReference2 = new AttachmentReference("", documentReference2);
  }
}
