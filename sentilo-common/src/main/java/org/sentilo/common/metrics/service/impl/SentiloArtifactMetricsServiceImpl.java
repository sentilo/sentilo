/*
 * Sentilo
 * 
 * Original version 1.4 Copyright (C) 2013 Institut Municipal d’Informàtica, Ajuntament de
 * Barcelona. Modified by Opentrends adding support for multitenant deployments and SaaS.
 * Modifications on version 1.5 Copyright (C) 2015 Opentrends Solucions i Sistemes, S.L.
 *
 * 
 * This program is licensed and may be used, modified and redistributed under the terms of the
 * European Public License (EUPL), either version 1.1 or (at your option) any later version as soon
 * as they are approved by the European Commission.
 * 
 * Alternatively, you may redistribute and/or modify this program under the terms of the GNU Lesser
 * General Public License as published by the Free Software Foundation; either version 3 of the
 * License, or (at your option) any later version.
 * 
 * Unless required by applicable law or agreed to in writing, software distributed under the License
 * is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
 * or implied.
 * 
 * See the licenses for the specific language governing permissions, limitations and more details.
 * 
 * You should have received a copy of the EUPL1.1 and the LGPLv3 licenses along with this program;
 * if not, you may find them at:
 * 
 * https://joinup.ec.europa.eu/software/page/eupl/licence-eupl http://www.gnu.org/licenses/ and
 * https://www.gnu.org/licenses/lgpl.txt
 */
package org.sentilo.common.metrics.service.impl;

import java.io.File;
import java.lang.management.ManagementFactory;
import java.lang.management.MemoryMXBean;
import java.lang.management.MemoryUsage;
import java.lang.management.ThreadMXBean;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.sentilo.common.metrics.SentiloArtifactMetrics;
import org.sentilo.common.metrics.repository.SentiloArtifactMetricsRepository;
import org.sentilo.common.metrics.service.SentiloArtifactMetricsService;
import org.sentilo.common.utils.ArtifactUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.Scheduled;

import com.sun.management.OperatingSystemMXBean;

@SuppressWarnings("restriction")
public abstract class SentiloArtifactMetricsServiceImpl implements SentiloArtifactMetricsService {

  public enum ArtifactState {
    RED, ORANGE, GREEN
  }

  protected static final String SUFFIX_KEY = "metrics";
  protected static final int INITIAL_DELAY = 30 * 1000; // 30 seconds
  protected static final int FIXED_DELAY = 30 * 1000; // 30 seconds

  @Autowired(required = false)
  private SentiloArtifactMetricsRepository repository;

  @Scheduled(initialDelay = INITIAL_DELAY, fixedDelay = FIXED_DELAY)
  public void collectAndSave() {
    final SentiloArtifactMetrics artifactMetrics = collect();
    saveArtifactMetrics(artifactMetrics);
  }

  public SentiloArtifactMetrics collect() {
    final String moduleKey = ArtifactUtils.buildUniqueArtifactKey(getName(), SUFFIX_KEY);
    final SentiloArtifactMetrics artifactMetrics = new SentiloArtifactMetrics(moduleKey);
    artifactMetrics.getMetrics().putAll(collectCommonMetrics());
    artifactMetrics.getMetrics().putAll(collectCustomMetrics());

    artifactMetrics.setState(getStatus(artifactMetrics).name());

    return artifactMetrics;
  }

  protected abstract Map<String, Object> collectCustomMetrics();

  protected abstract String getName();

  /** Returns a code that clearly identifies artifact state */
  protected abstract ArtifactState getStatus(final SentiloArtifactMetrics artifactMetrics);

  protected void saveArtifactMetrics(final SentiloArtifactMetrics artifactMetrics) {
    repository.saveArtifactMetrics(artifactMetrics);
  }

  protected Map<String, Object> collectCommonMetrics() {
    final Map<String, Object> metrics = new HashMap<String, Object>();

    // Global system metrics
    final OperatingSystemMXBean osBean = ManagementFactory.getPlatformMXBean(OperatingSystemMXBean.class);

    metrics.put(CPU_METRICS, collectCpu(osBean));
    metrics.put(SYSTEM_MEM_METRICS, collectSystemMemory(osBean));
    metrics.put(PROCESS_MEM_METRICS, collectProcessMemory());
    metrics.put(PROCESS_THREADS_METRICS, collectProcessThreads());
    metrics.put(FS_METRICS, collectFileSystem());

    return metrics;
  }

  private Map<String, Object> collectCpu(final OperatingSystemMXBean osBean) {
    // System CPU information:
    // - Number of processors
    // - What % CPU load this current JVM is taking, from 0.0-1.0
    // - What % load the overall system is at, from 0.0-1.0
    final Map<String, Object> cpu = new HashMap<String, Object>();
    cpu.put(CPU_NUMBER, Runtime.getRuntime().availableProcessors());
    cpu.put(CPU_PROCESS_LOAD, osBean.getProcessCpuLoad());
    cpu.put(CPU_SYSTEM_LOAD, osBean.getSystemCpuLoad());

    return cpu;
  }

  private Map<String, Object> collectSystemMemory(final OperatingSystemMXBean osBean) {
    final long free = osBean.getFreePhysicalMemorySize();
    final long total = osBean.getTotalPhysicalMemorySize();
    final long used = total - free;

    final Map<String, Object> systemMemory = new HashMap<String, Object>();
    systemMemory.put(SYSTEM_MEM_TOTAL, total);
    systemMemory.put(SYSTEM_MEM_FREE, free);
    systemMemory.put(SYSTEM_MEM_USED, used);

    return systemMemory;
  }

  private Map<String, Object> collectProcessMemory() {
    final MemoryMXBean memBean = ManagementFactory.getMemoryMXBean();
    final MemoryUsage heapMemUsage = memBean.getHeapMemoryUsage();
    final MemoryUsage nonHeapMemUsage = memBean.getNonHeapMemoryUsage();

    final Map<String, Object> processMemory = new HashMap<String, Object>();
    final Map<String, Object> heap = new HashMap<String, Object>();
    final Map<String, Object> nonHeap = new HashMap<String, Object>();
    heap.put(PROCESS_MEM_INIT, heapMemUsage.getInit());
    heap.put(PROCESS_MEM_USED, heapMemUsage.getUsed());
    heap.put(PROCESS_MEM_COMMITTED, heapMemUsage.getCommitted());
    heap.put(PROCESS_MEM_MAX, heapMemUsage.getMax());
    nonHeap.put(PROCESS_MEM_INIT, nonHeapMemUsage.getInit());
    nonHeap.put(PROCESS_MEM_USED, nonHeapMemUsage.getUsed());
    nonHeap.put(PROCESS_MEM_COMMITTED, nonHeapMemUsage.getCommitted());
    nonHeap.put(PROCESS_MEM_MAX, nonHeapMemUsage.getMax());

    processMemory.put(PROCESS_HEAP_MEM_METRICS, heap);
    processMemory.put(PROCESS_NON_HEAP_MEM_METRICS, nonHeap);

    return processMemory;
  }

  private Map<String, Object> collectProcessThreads() {
    final ThreadMXBean threadBean = ManagementFactory.getThreadMXBean();

    final Map<String, Object> threads = new HashMap<String, Object>();
    threads.put(PROCESS_THREADS_TOTAL, threadBean.getThreadCount());
    threads.put(PROCESS_THREADS_DAEMON, threadBean.getDaemonThreadCount());
    threads.put(PROCESS_THREADS_STARTED, threadBean.getTotalStartedThreadCount());

    return threads;
  }

  private List<Object> collectFileSystem() {
    /* Get a list of all filesystem roots on this system */
    final File[] roots = File.listRoots();
    /* For each filesystem root, print some info */
    final List<Object> filesystems = new ArrayList<Object>();
    for (final File root : roots) {
      final Map<String, Object> filesystem = new HashMap<String, Object>();
      filesystem.put(FS_PATH, root.getAbsolutePath());
      filesystem.put(FS_TOTAL, root.getTotalSpace());
      filesystem.put(FS_FREE, root.getFreeSpace());
      filesystem.put(FS_USABLE, root.getUsableSpace());

      filesystems.add(filesystem);
    }

    return filesystems;
  }
}
