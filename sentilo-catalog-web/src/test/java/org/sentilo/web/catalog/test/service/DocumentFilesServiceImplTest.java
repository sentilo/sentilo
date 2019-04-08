package org.sentilo.web.catalog.test.service;

import static org.junit.Assert.assertEquals;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Optional;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.sentilo.web.catalog.domain.DocumentFile;
import org.sentilo.web.catalog.repository.DocumentFileRepository;
import org.sentilo.web.catalog.service.impl.DocumentFilesServiceImpl;

public class DocumentFilesServiceImplTest extends AbstractBaseCrudServiceImplTest {

  private final String documentFileId = "doc-1";

  @InjectMocks
  private DocumentFilesServiceImpl service;

  @Mock
  private DocumentFileRepository repository;

  @Mock
  private DocumentFile documentFile;

  @Before
  public void setUp() {
    MockitoAnnotations.initMocks(this);
    service.init();
  }

  @Test
  public void getEntityId() {
    when(documentFile.getId()).thenReturn(documentFileId);

    assertEquals(documentFileId, service.getEntityId(documentFile));
  }

  @Test
  public void checkDuplicateFilename() {
    final String entityId = "mockEntity";
    final String filename = "mockFile";
    final String documentFileId = DocumentFile.buildId(entityId, filename);
    when(repository.findById(documentFileId)).thenReturn(Optional.of(documentFile));

    service.checkDuplicateFilename(entityId, filename);

    verify(repository).findById(documentFileId);
  }

}
