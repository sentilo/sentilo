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
package org.sentilo.common.metrics.service;

public interface SentiloArtifactMetricsService {

  public static final String CPU_METRICS = "cpu";
  public static final String CPU_NUMBER = "number";
  public static final String CPU_SYSTEM_LOAD = "system_load";
  public static final String CPU_PROCESS_LOAD = "process_load";

  public static final String SYSTEM_MEM_METRICS = "system_memory";
  public static final String SYSTEM_MEM_TOTAL = "total";
  public static final String SYSTEM_MEM_FREE = "free";
  public static final String SYSTEM_MEM_USED = "used";

  public static final String PROCESS_MEM_METRICS = "process_memory";
  public static final String PROCESS_HEAP_MEM_METRICS = "heap";
  public static final String PROCESS_NON_HEAP_MEM_METRICS = "non_heap";
  public static final String PROCESS_MEM_INIT = "init";
  public static final String PROCESS_MEM_USED = "used";
  public static final String PROCESS_MEM_COMMITTED = "committed";
  public static final String PROCESS_MEM_MAX = "max";

  public static final String PROCESS_THREADS_METRICS = "threads";
  public static final String PROCESS_THREADS_TOTAL = "total";
  public static final String PROCESS_THREADS_DAEMON = "daemon";
  public static final String PROCESS_THREADS_STARTED = "started";

  public static final String FS_METRICS = "file_systems";
  public static final String FS_PREFIX = "fs_";
  public static final String FS_PATH = "path";
  public static final String FS_TOTAL = "total";
  public static final String FS_FREE = "free";
  public static final String FS_USABLE = "usable";

  public static final String REDIS_POOL_METRICS = "redis_pool";
  public static final String REDIS_POOL_NUM_ACTIVE = "num_active";
  public static final String REDIS_POOL_NUM_IDLE = "num_idle";
  public static final String REDIS_POOL_NUM_WAITERS = "num_waiters";
  public static final String REDIS_POOL_MAX_BORROW_TS = "max_borrow_wait_ts";
  public static final String REDIS_POOL_MEAN_BORROW_TS = "mean_borrow_wait_ts";

  public static final String STATUS_OK = "OK";
  public static final String STATUS_KO = "KO";

  void collectAndSave();
}
