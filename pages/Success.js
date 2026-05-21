import { useNavigate } from "react-router-dom";

export default function Success() {
  const nav = useNavigate();

  return (
    <div className="auth-container">
      <div className="auth-box success-box" style={{ textAlign: "center" }}>
        <div style={{ fontSize: "60px" }}>✅</div>

        <h2>Password reset successful</h2>

        <p style={{ margin: "10px 0 20px", color: "gray" }}>
          Your password has been changed successfully.
          Please sign in with your new password.
        </p>

        <button onClick={() => nav("/login")}>
          Login
        </button>
      </div>
    </div>
  );
}