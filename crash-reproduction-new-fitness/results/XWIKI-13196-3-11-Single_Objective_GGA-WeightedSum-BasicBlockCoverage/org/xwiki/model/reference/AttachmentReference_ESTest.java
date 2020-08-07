/*
 * This file was automatically generated by EvoSuite
 * Thu May 14 20:19:21 UTC 2020
 */

package org.xwiki.model.reference;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;
import org.xwiki.model.reference.AttachmentReference;
import org.xwiki.model.reference.DocumentReference;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class AttachmentReference_ESTest extends AttachmentReference_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      DocumentReference documentReference0 = new DocumentReference("Minimum abbreviation width is 4", "pvQ_WV'-TLuy#T\"1w7", "Minimum abbreviation width is 4", "Minimum abbreviation width is 4");
      AttachmentReference attachmentReference0 = new AttachmentReference("] for an attachment reference", documentReference0);
      attachmentReference0.removeParent(documentReference0);
      AttachmentReference attachmentReference1 = new AttachmentReference("", documentReference0);
  }
}
