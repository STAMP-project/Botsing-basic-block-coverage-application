/*
 * This file was automatically generated by EvoSuite
 * Sun May 17 20:42:10 UTC 2020
 */

package org.xwiki.notifications.internal.email;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import java.util.LinkedList;
import java.util.PriorityQueue;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;
import org.slf4j.event.SubstituteLoggingEvent;
import org.slf4j.helpers.SubstituteLogger;
import org.xwiki.notifications.internal.email.PeriodicMimeMessageIterator;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class AbstractMimeMessageIterator_ESTest extends AbstractMimeMessageIterator_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      PeriodicMimeMessageIterator periodicMimeMessageIterator0 = new PeriodicMimeMessageIterator();
      LinkedList<SubstituteLoggingEvent> linkedList0 = new LinkedList<SubstituteLoggingEvent>();
      PriorityQueue<SubstituteLoggingEvent> priorityQueue0 = new PriorityQueue<SubstituteLoggingEvent>();
      SubstituteLogger substituteLogger0 = new SubstituteLogger("XWiki", linkedList0, false);
      // Undeclared exception!
      periodicMimeMessageIterator0.next();
  }
}
