/**
 * Base error class for Quickslice client errors
 */
export class QuicksliceError extends Error {
  constructor(message: string) {
    super(message);
    this.name = 'QuicksliceError';
  }
}

/**
 * Thrown when authentication is required but user is not logged in
 */
export class LoginRequiredError extends QuicksliceError {
  constructor(message = 'Login required') {
    super(message);
    this.name = 'LoginRequiredError';
  }
}

/**
 * Thrown when network request fails
 */
export class NetworkError extends QuicksliceError {
  constructor(message: string) {
    super(message);
    this.name = 'NetworkError';
  }
}

/**
 * Thrown when OAuth flow fails
 */
export class OAuthError extends QuicksliceError {
  public code: string;
  public description?: string;

  constructor(code: string, description?: string) {
    super(`OAuth error: ${code}${description ? ` - ${description}` : ''}`);
    this.name = 'OAuthError';
    this.code = code;
    this.description = description;
  }
}
