package org.sentilo.web.catalog.test.s3.zenko.client;

import java.io.IOException;
import java.net.URL;

import com.amazonaws.AmazonServiceException;
import com.amazonaws.HttpMethod;
import com.amazonaws.SdkClientException;
import com.amazonaws.auth.profile.ProfileCredentialsProvider;
import com.amazonaws.client.builder.AwsClientBuilder.EndpointConfiguration;
import com.amazonaws.services.s3.AmazonS3;
import com.amazonaws.services.s3.AmazonS3ClientBuilder;
import com.amazonaws.services.s3.model.GeneratePresignedUrlRequest;

public class AwsUrlGeneratorTest {

  public static void main(final String[] args) throws IOException {
    final String bucketName = "testbucket";
    final String objectKey = "sample.mp3";

    try {
      final AmazonS3 s3Client = AmazonS3ClientBuilder.standard().withPathStyleAccessEnabled(true).withCredentials(new ProfileCredentialsProvider())
          .withEndpointConfiguration(new EndpointConfiguration("http://192.168.2.181:8000", "eu-west-3")).build();

      // Set the presigned URL to expire after one hour.
      final java.util.Date expiration = new java.util.Date();
      long expTimeMillis = expiration.getTime();
      expTimeMillis += 1000 * 60 * 60;
      expiration.setTime(expTimeMillis);

      // Generate the presigned URL.
      System.out.println("Generating pre-signed URL.");
      final GeneratePresignedUrlRequest generatePresignedUrlRequest =
          new GeneratePresignedUrlRequest(bucketName, objectKey).withMethod(HttpMethod.GET).withExpiration(expiration);

      final URL url = s3Client.generatePresignedUrl(generatePresignedUrlRequest);

      System.out.println("Pre-Signed URL: " + url.toString());

    } catch (final AmazonServiceException e) {
      // The call was transmitted successfully, but Amazon S3 couldn't process
      // it, so it returned an error response.
      e.printStackTrace();
    } catch (final SdkClientException e) {
      // Amazon S3 couldn't be contacted for a response, or the client
      // couldn't parse the response from Amazon S3.
      e.printStackTrace();
    }
  }

}
