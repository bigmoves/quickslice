/**
 * Base error class for Quickslice client errors
 */
export declare class QuicksliceError extends Error {
    constructor(message: string);
}
/**
 * Thrown when authentication is required but user is not logged in
 */
export declare class LoginRequiredError extends QuicksliceError {
    constructor(message?: string);
}
/**
 * Thrown when network request fails
 */
export declare class NetworkError extends QuicksliceError {
    constructor(message: string);
}
/**
 * Thrown when OAuth flow fails
 */
export declare class OAuthError extends QuicksliceError {
    code: string;
    description?: string;
    constructor(code: string, description?: string);
}
